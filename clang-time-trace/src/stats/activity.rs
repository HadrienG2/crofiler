//! Handling of activities, that is, clang-provided categories of stuff it can
//! be doing at a point in time

use super::ArgParseError;
use crate::{
    ctf::{events::duration::DurationEvent, Duration, Timestamp, TraceEvent},
    PathError, PathKey,
};
use cpp_demangle::{DemangleOptions, ParseOptions, Symbol};
use cpparser::{nom, EntityKey, EntityParser};
use log::trace;
use serde_json as json;
use std::{cell::RefCell, collections::HashMap};
use thiserror::Error;

/// Tuned 10x above maximum observed requirement
const CPP_DEMANGLE_RECURSION_LIMIT: u32 = 1024;

/// Clang activity with timing information
#[derive(Clone, Debug, PartialEq)]
pub struct ActivityStat {
    /// What clang was doing
    activity: Activity,

    /// When it started doing it
    start: Timestamp,

    /// How long it did it
    duration: Duration,
}
//
impl ActivityStat {
    /// Create an ActivityStat from scratch
    pub fn new(activity: Activity, start: Timestamp, duration: Timestamp) -> Self {
        Self {
            activity,
            start,
            duration,
        }
    }

    /// Decode a TraceEvent which is expected to contain a timed activity
    pub fn parse(
        t: TraceEvent,
        parser: &EntityParser,
        demangling_buf: &mut String,
    ) -> Result<Self, ActivityStatParseError> {
        match t {
            TraceEvent::X {
                duration_event:
                    DurationEvent {
                        pid: _,
                        tid: _,
                        ts,
                        name: Some(name),
                        cat: None,
                        tts: None,
                        args,
                        stack_trace: None,
                    },
                dur,
                tdur: None,
                end_stack_trace: None,
            } => {
                let activity = Activity::parse(name, args, parser, demangling_buf)?;
                Ok(Self {
                    activity,
                    start: ts,
                    duration: dur,
                })
            }
            _ => Err(ActivityStatParseError::UnexpectedInput(t)),
        }
    }

    /// What clang was doing
    pub fn activity(&self) -> &Activity {
        &self.activity
    }

    /// When it started doing it
    pub fn start(&self) -> Timestamp {
        self.start
    }

    /// How long it did it
    pub fn duration(&self) -> Duration {
        self.duration
    }

    /// When it stopped doing it
    pub fn end(&self) -> Timestamp {
        self.start + self.duration
    }
}

/// What can go wrong while parsing an activity profile
#[derive(Error, Debug, PartialEq)]
pub enum ActivityStatParseError {
    /// Encountered unexpected input
    #[error("attempted to parse ActivityStat from unexpected {0:#?}")]
    UnexpectedInput(TraceEvent),

    /// Failed to parse inner activity
    #[error("failed to parse activity ({0})")]
    BadArguments(#[from] ActivityParseError),
}

/// Activity that Clang can engage in during the compilation process
// FIXME: Replace this enum with a perfect hash map
#[derive(Clone, Debug, PartialEq)]
pub enum Activity {
    /// Processing a source file
    Source(PathKey),

    /// Parsing a class
    ParseClass(EntityKey),

    /// Instantiating a class
    InstantiateClass(EntityKey),

    /// Parsing a template
    ParseTemplate(EntityKey),

    /// Instantiating a function
    InstantiateFunction(EntityKey),

    /// Generating debug info for a type
    DebugType(EntityKey),

    /// Generating debug info for a global variable
    DebugGlobalVariable(EntityKey),

    /// Generate a function's code
    CodeGenFunction(EntityKey),

    /// Generating debug info for a function
    DebugFunction(EntityKey),

    /// Perform pending instantiations (as the name suggests)
    PerformPendingInstantiations,

    /// Compiler front-end work
    Frontend,

    /// Running a named compiler pass
    RunPass(Box<str>),

    /// Optimizing code
    OptFunction(MangledSymbol),

    /// Per-function compiler passes
    PerFunctionPasses,

    /// Running a named loop compiler pass
    RunLoopPass(Box<str>),

    /// Optimizing a module
    OptModule(PathKey),

    /// Per-module compiler passes
    PerModulePasses,

    /// Code generation passes
    CodeGenPasses,

    /// Compiler back-end work
    Backend,

    /// Compiler execution
    ExecuteCompiler,

    // FIXME: Add new tests for these new passes
    //
    /// Infer function attributes
    InferFunctionAttrsPass(PathKey),

    /// Function pass manager (aka `PassManager<llvm::Function>`)
    FunctionPassManager(Option<MangledSymbol>),

    /// Scalar replacement of aggregates pass
    SROAPass(MangledSymbol),

    /// Module-to-function pass adaptor
    ModuleToFunctionPassAdaptor(PathKey),

    /// Interprocedural Sparse Conditional Constant Propagation pass
    IPSCCPPass(PathKey),

    /// Called value propagation pass
    CalledValuePropagationPass(PathKey),

    /// Global optimization pass
    GlobalOptPass(PathKey),

    /// Promotion pass
    PromotePass(MangledSymbol),

    /// Dead argument elimination pass
    DeadArgumentEliminationPass(PathKey),

    /// Instruction combination pass
    InstCombinePass(MangledSymbol),

    /// Alias analysis pass for globals (aka `RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>`)
    ModuleGlobalsAAPass(PathKey),

    /// Adaptor that maps from a SCC to its functions
    CGSCCToFunctionPassAdaptor,

    /// A helper that repeats an SCC pass each time an indirect call is refined to a direct call by that pass
    DevirtSCCRepeatedPass,

    /// Function to loop pass adaptor
    FunctionToLoopPassAdaptor(MangledSymbol),

    /// Inliner pass
    InlinerPass,

    /// Jump threading pass
    JumpThreadingPass(MangledSymbol),

    /// Loop full unrolling pass
    LoopFullUnrollPass,

    /// Loop analysis manager
    ///
    /// aka `PassManager<llvm::Loop, llvm::LoopAnalysisManager, llvm::LoopStandardAnalysisResults &, llvm::LPMUpdater &>`
    ///
    LoopAnalysisManager,

    /// Global Value Numbering pass
    GVNPass(MangledSymbol),

    /// Early Common Subexpression Elimination pass
    EarlyCSEPass(MangledSymbol),

    /// Memcpy optimization pass
    MemCpyOptPass(MangledSymbol),

    /// Correlated value propagation pass
    CorrelatedValuePropagationPass(MangledSymbol),

    /// Control flow graph simplification pass
    SimplifyCFGPass(MangledSymbol),

    /// Tail call elimination pass
    TailCallElimPass(MangledSymbol),

    /// Commutative expression reassociation pass
    ReassociatePass(MangledSymbol),

    /// Loop rotation pass
    LoopRotatePass,

    /// Loop invariant code motion pass
    LICMPass,

    /// Function-level constant propagation and merging pass
    SCCPPass(MangledSymbol),

    /// Bit-tracking dead code elimination pass
    BDCEPass(MangledSymbol),

    /// DCE pass that assumes instructions are dead until proven otherwise
    ADCEPass(MangledSymbol),

    /// Dead Store Elimination pass
    DSEPass(MangledSymbol),

    /// Induction variable simplification pass
    IndVarSimplifyPass,

    /// Pass that computes function attributes in post-order over the call graph
    PostOrderFunctionAttrsPass,

    /// Loop canonicalization pass
    LoopSimplifyPass(MangledSymbol),

    /// Loop instruction simplification pass
    LoopInstSimplifyPass,

    /// Pass that does a post-order walk of the SCCs and runs a CGSCC pass over each
    ModuleToPostOrderCGSCCPassAdaptor(PathKey),

    /// Module pass wrapping the inliner pass
    ModuleInlinerWrapperPass(PathKey),

    /// Global Dead Code Elimination pass
    GlobalDCEPass(PathKey),

    /// Eliminate available external globals
    EliminateAvailableExternallyPass(PathKey),

    /// Pass that walks SCCs of the call graph in RPO to deduce and propagate function attributes
    ReversePostOrderFunctionAttrsPass(PathKey),

    /// SLP vectorizer pass (tries to vectorize store sequences)
    SLPVectorizerPass(MangledSymbol),

    /// Pass that demotes FP operations to work on integers when losslessly possible
    Float2IntPass(MangledSymbol),

    /// Loop vectorization pass
    LoopVectorizePass(MangledSymbol),

    /// Constant instrinsics lowering pass
    LowerConstantIntrinsicsPass(MangledSymbol),

    /// Loop unrolling pass
    LoopUnrollPass(MangledSymbol),

    /// Pass that performes profile-guided sinking of instructions into loops
    LoopSinkPass(MangledSymbol),

    /// Pass that runs instruction simplification across each instruction in the function
    InstSimplifyPass(MangledSymbol),

    /// Loop load elimination pass
    LoopLoadEliminationPass(MangledSymbol),

    /// Pass that populates the VFABI attribute with scalar-to-vector mappings from TargetLibraryInfo
    InjectTLIMappings(MangledSymbol),

    /// Pass that optimizes scalar/vector interactions in IR using target cost models
    VectorCombinePass(MangledSymbol),

    /// Call Graph Profile pass
    CGProfilePass(PathKey),

    /// Constant deduplication pass
    ConstantMergePass(PathKey),

    /// Pass that converts lookup tables to relative lookup tables for PIC-friendliness
    RelLookupTableConverterPass(PathKey),

    /// Top-level optimizer activity?
    Optimizer,
}
//
impl Activity {
    /// Textual name of the activity, as featured in the JSON data
    pub fn name(&self) -> &'static str {
        use Activity::*;
        match self {
            Source(_) => "Source",
            ParseClass(_) => "ParseClass",
            InstantiateClass(_) => "InstantiateClass",
            ParseTemplate(_) => "ParseTemplate",
            InstantiateFunction(_) => "InstantiateFunction",
            DebugType(_) => "DebugType",
            DebugGlobalVariable(_) => "DebugGlobalVariable",
            CodeGenFunction(_) => "CodeGen Function",
            DebugFunction(_) => "DebugFunction",
            PerformPendingInstantiations => "PerformPendingInstantiations",
            Frontend => "Frontend",
            RunPass(_) => "RunPass",
            OptFunction(_) => "OptFunction",
            PerFunctionPasses => "PerFunctionPasses",
            RunLoopPass(_) => "RunLoopPass",
            OptModule(_) => "OptModule",
            PerModulePasses => "PerModulePasses",
            CodeGenPasses => "CodeGenPasses",
            Backend => "Backend",
            ExecuteCompiler => "ExecuteCompiler",
            InferFunctionAttrsPass(_) => "InferFunctionAttrsPass",
            FunctionPassManager(_) => "PassManager<llvm::Function>",
            SROAPass(_) => "SROAPass",
            ModuleToFunctionPassAdaptor(_) => "ModuleToFunctionPassAdaptor",
            IPSCCPPass(_) => "IPSCCPPass",
            CalledValuePropagationPass(_) => "CalledValuePropagationPass",
            GlobalOptPass(_) => "GlobalOptPass",
            PromotePass(_) => "PromotePass",
            DeadArgumentEliminationPass(_) => "DeadArgumentEliminationPass",
            InstCombinePass(_) => "InstCombinePass",
            ModuleGlobalsAAPass(_) => "RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>",
            CGSCCToFunctionPassAdaptor => "CGSCCToFunctionPassAdaptor",
            DevirtSCCRepeatedPass => "DevirtSCCRepeatedPass",
            FunctionToLoopPassAdaptor(_) => "FunctionToLoopPassAdaptor",
            InlinerPass => "InlinerPass",
            JumpThreadingPass(_) => "JumpThreadingPass",
            LoopFullUnrollPass => "LoopFullUnrollPass(<unnamed loop>)",
            LoopAnalysisManager => "PassManager<llvm::Loop, llvm::LoopAnalysisManager, llvm::LoopStandardAnalysisResults &, llvm::LPMUpdater &>",
            GVNPass(_) => "GVNPass",
            EarlyCSEPass(_) => "EarlyCSEPass",
            MemCpyOptPass(_) => "MemCpyOptPass",
            CorrelatedValuePropagationPass(_) => "CorrelatedValuePropagationPass",
            SimplifyCFGPass(_) => "SimplifyCFGPass",
            TailCallElimPass(_) => "TailCallElimPass",
            ReassociatePass(_) => "ReassociatePass",
            LoopRotatePass => "LoopRotatePass",
            LICMPass => "LICMPass",
            SCCPPass(_) => "SCCPPass",
            BDCEPass(_) => "BDCEPass",
            ADCEPass(_) => "ADCEPass",
            DSEPass(_) => "DSEPass",
            IndVarSimplifyPass => "IndVarSimplifyPass",
            PostOrderFunctionAttrsPass => "PostOrderFunctionAttrsPass",
            LoopSimplifyPass(_) => "LoopSimplifyPass",
            LoopInstSimplifyPass => "LoopInstSimplifyPass",
            ModuleToPostOrderCGSCCPassAdaptor(_) => "ModuleToPostOrderCGSCCPassAdaptor",
            ModuleInlinerWrapperPass(_) => "ModuleInlinerWrapperPass",
            GlobalDCEPass(_) => "GlobalDCEPass",
            EliminateAvailableExternallyPass(_) => "EliminateAvailableExternallyPass",
            ReversePostOrderFunctionAttrsPass(_) => "ReversePostOrderFunctionAttrsPass",
            SLPVectorizerPass(_) => "SLPVectorizerPass",
            Float2IntPass(_) => "Float2IntPass",
            LoopVectorizePass(_) => "LoopVectorizePass",
            LowerConstantIntrinsicsPass(_) => "LowerConstantIntrinsicsPass",
            LoopUnrollPass(_) => "LoopUnrollPass",
            LoopSinkPass(_) => "LoopSinkPass",
            InstSimplifyPass(_) => "InstSimplifyPass",
            LoopLoadEliminationPass(_) => "LoopLoadEliminationPass",
            InjectTLIMappings(_) => "InjectTLIMappings",
            VectorCombinePass(_) => "VectorCombinePass",
            CGProfilePass(_) => "CGProfilePass",
            ConstantMergePass(_) => "ConstantMergePass",
            RelLookupTableConverterPass(_) => "RelLookupTableConverterPass",
            Optimizer => "Optimizer",
        }
    }

    /// Parsed argument of the activity
    pub fn argument(&self) -> ActivityArgument {
        use Activity::*;
        use ActivityArgument::*;
        match self {
            PerformPendingInstantiations
            | Frontend
            | PerFunctionPasses
            | PerModulePasses
            | CodeGenPasses
            | Backend
            | ExecuteCompiler
            | CGSCCToFunctionPassAdaptor
            | DevirtSCCRepeatedPass
            | InlinerPass
            | LoopFullUnrollPass
            | LoopAnalysisManager
            | LoopRotatePass
            | LICMPass
            | IndVarSimplifyPass
            | PostOrderFunctionAttrsPass
            | LoopInstSimplifyPass
            | Optimizer => Nothing,
            RunPass(s) | RunLoopPass(s) => String(s.clone()),
            Source(p)
            | OptModule(p)
            | InferFunctionAttrsPass(p)
            | ModuleToFunctionPassAdaptor(p)
            | IPSCCPPass(p)
            | CalledValuePropagationPass(p)
            | GlobalOptPass(p)
            | DeadArgumentEliminationPass(p)
            | ModuleGlobalsAAPass(p)
            | ModuleToPostOrderCGSCCPassAdaptor(p)
            | ModuleInlinerWrapperPass(p)
            | GlobalDCEPass(p)
            | EliminateAvailableExternallyPass(p)
            | ReversePostOrderFunctionAttrsPass(p)
            | CGProfilePass(p)
            | ConstantMergePass(p)
            | RelLookupTableConverterPass(p) => FilePath(*p),
            ParseClass(e)
            | InstantiateClass(e)
            | ParseTemplate(e)
            | InstantiateFunction(e)
            | DebugType(e)
            | DebugGlobalVariable(e)
            | CodeGenFunction(e)
            | DebugFunction(e) => CppEntity(e.clone()),
            OptFunction(m)
            | SROAPass(m)
            | PromotePass(m)
            | InstCombinePass(m)
            | FunctionToLoopPassAdaptor(m)
            | JumpThreadingPass(m)
            | GVNPass(m)
            | EarlyCSEPass(m)
            | MemCpyOptPass(m)
            | CorrelatedValuePropagationPass(m)
            | SimplifyCFGPass(m)
            | TailCallElimPass(m)
            | ReassociatePass(m)
            | SCCPPass(m)
            | BDCEPass(m)
            | ADCEPass(m)
            | DSEPass(m)
            | LoopSimplifyPass(m)
            | SLPVectorizerPass(m)
            | Float2IntPass(m)
            | LoopVectorizePass(m)
            | LowerConstantIntrinsicsPass(m)
            | LoopUnrollPass(m)
            | LoopSinkPass(m)
            | InstSimplifyPass(m)
            | LoopLoadEliminationPass(m)
            | InjectTLIMappings(m)
            | VectorCombinePass(m) => MangledSymbol(m.clone()),
            FunctionPassManager(o) => MangledSymbolOpt(o.clone()),
        }
    }

    /// Parse from useful bits of Duration events
    fn parse(
        name: Box<str>,
        args: Option<HashMap<Box<str>, json::Value>>,
        parser: &EntityParser,
        demangling_buf: &mut String,
    ) -> Result<Self, ActivityParseError> {
        // Interior mutability to allow multiple mutable borrows
        let args = RefCell::new(args);

        // Handling of activities with no arguments
        let no_args = |a: Activity| -> Result<Activity, ActivityParseError> {
            Self::parse_empty_args(args.borrow_mut().take())?;
            Ok(a)
        };

        // Handling of activities with one "detail" argument
        let detail_arg = || -> Result<Box<str>, ArgParseError> {
            Self::parse_detail_arg(args.borrow_mut().take())
        };
        //
        let fill_str_arg =
            |constructor: fn(Box<str>) -> Activity| -> Result<Activity, ActivityParseError> {
                Ok(constructor(detail_arg()?))
            };
        //
        let fill_path_arg =
            |constructor: fn(PathKey) -> Activity| -> Result<Activity, ActivityParseError> {
                let path: &str = &detail_arg()?;
                Ok(constructor(parser.path_to_key(path)))
            };
        //
        let parse_entity = |s: &str| -> Result<EntityKey, ActivityParseError> {
            parser.parse_entity(s).map_err(|e| {
                ActivityParseError::from(nom::error::Error::new(Box::<str>::from(e.input), e.code))
            })
        };
        //
        let fill_entity_arg =
            |constructor: fn(EntityKey) -> Activity| -> Result<Activity, ActivityParseError> {
                let entity: &str = &detail_arg()?;
                Ok(constructor(parse_entity(entity)?))
            };
        //
        let demangling_buf = RefCell::new(demangling_buf);
        let mangled_arg = || -> Result<MangledSymbol, ActivityParseError> {
            let symbol = detail_arg()?;
            let mut demangling_buf = demangling_buf.borrow_mut();

            let parse_demangled = |entity: Box<str>| -> MangledSymbol {
                if let Ok(parsed) = parse_entity(&*entity) {
                    MangledSymbol::Parsed(parsed)
                } else {
                    MangledSymbol::Demangled(entity)
                }
            };

            let demangling_result = match Symbol::new_with_options(
                &*symbol,
                &ParseOptions::default().recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
            )
            .map(|s| {
                demangling_buf.clear();
                s.structured_demangle(
                    &mut *demangling_buf,
                    &DemangleOptions::default()
                        .hide_expression_literal_types()
                        .no_return_type()
                        .recursion_limit(CPP_DEMANGLE_RECURSION_LIMIT),
                )
            }) {
                // Mangled symbol was successfully demangled, intern it along with the rest
                Ok(Ok(())) => parse_demangled(demangling_buf.clone().into_boxed_str()),

                // Symbol failed to demangle, try some patterns that cpp_demangle
                // should not reject but actually does reject before giving up
                Ok(Err(_)) | Err(_) => match &*symbol {
                    "main" | "__clang_call_terminate" => parse_demangled(symbol),
                    _ => MangledSymbol::Mangled(symbol),
                },
            };

            match &demangling_result {
                MangledSymbol::Parsed(_) => {}
                MangledSymbol::Demangled(d) => trace!("Failed to parse demangled symbol {d:?}"),
                MangledSymbol::Mangled(m) => trace!("Failed to demangle symbol {m:?}"),
            }

            Ok(demangling_result)
        };
        //
        let fill_mangled_arg =
            |constructor: fn(MangledSymbol) -> Activity| -> Result<Activity, ActivityParseError> {
                Ok(constructor(mangled_arg()?))
            };
        //
        let fill_mangled_arg_opt =
            |constructor: fn(Option<MangledSymbol>) -> Activity| -> Result<Activity, ActivityParseError> {
                match mangled_arg() {
                    Ok(arg) => Ok(constructor(Some(arg))),
                    Err(ActivityParseError::BadArguments(ArgParseError::MissingKey("detail"))) => Ok(constructor(None)),
                    Err(e) => Err(e)?
                }
            };
        //
        let unnamed_loop_arg = |constructor: Activity| -> Result<Activity, ActivityParseError> {
            let loop_name = detail_arg()?;
            if &*loop_name == "<unnamed loop>" {
                Ok(constructor)
            } else {
                Err(ActivityParseError::UnexpectedLoopName(loop_name))
            }
        };
        //
        let unnamed_loop_arg_opt = |constructor: Activity| -> Result<Activity, ActivityParseError> {
            match detail_arg() {
                Ok(loop_name) => {
                    if &*loop_name == "<unnamed loop>" {
                        Ok(constructor)
                    } else {
                        Err(ActivityParseError::UnexpectedLoopName(loop_name))
                    }
                }
                Err(ArgParseError::MissingKey("detail")) => Ok(constructor),
                Err(e) => Err(e)?,
            }
        };

        // Parse the activity name and parse arguments accordingly
        use Activity::*;
        let result = match &*name {
            "PerformPendingInstantiations" => no_args(PerformPendingInstantiations),
            "Frontend" => no_args(Frontend),
            "PerFunctionPasses" => no_args(PerFunctionPasses),
            "PerModulePasses" => no_args(PerModulePasses),
            "CodeGenPasses" => no_args(CodeGenPasses),
            "Backend" => no_args(Backend),
            "ExecuteCompiler" => no_args(ExecuteCompiler),
            "Source" => fill_path_arg(Source),
            "ParseClass" => fill_entity_arg(ParseClass),
            "InstantiateClass" => fill_entity_arg(InstantiateClass),
            "ParseTemplate" => fill_entity_arg(ParseTemplate),
            "InstantiateFunction" => fill_entity_arg(InstantiateFunction),
            "DebugType" => fill_entity_arg(DebugType),
            "DebugGlobalVariable" => fill_entity_arg(DebugGlobalVariable),
            "CodeGen Function" => fill_entity_arg(CodeGenFunction),
            "DebugFunction" => fill_entity_arg(DebugFunction),
            "RunPass" => fill_str_arg(RunPass),
            "OptFunction" => fill_mangled_arg(OptFunction),
            "RunLoopPass" => fill_str_arg(RunLoopPass),
            "OptModule" => fill_path_arg(OptModule),
            "InferFunctionAttrsPass" => fill_path_arg(InferFunctionAttrsPass),
            "PassManager<llvm::Function>" => fill_mangled_arg_opt(FunctionPassManager),
            "SROAPass" => fill_mangled_arg(SROAPass),
            "ModuleToFunctionPassAdaptor" => fill_path_arg(ModuleToFunctionPassAdaptor),
            "IPSCCPPass" => fill_path_arg(IPSCCPPass),
            "CalledValuePropagationPass" => fill_path_arg(CalledValuePropagationPass),
            "GlobalOptPass" => fill_path_arg(GlobalOptPass),
            "PromotePass" => fill_mangled_arg(PromotePass),
            "DeadArgumentEliminationPass" => fill_path_arg(DeadArgumentEliminationPass),
            "InstCombinePass" => fill_mangled_arg(InstCombinePass),
            "RequireAnalysisPass<llvm::GlobalsAA, llvm::Module>" => {
                fill_path_arg(ModuleGlobalsAAPass)
            }
            "CGSCCToFunctionPassAdaptor" => no_args(CGSCCToFunctionPassAdaptor),
            "DevirtSCCRepeatedPass" => no_args(DevirtSCCRepeatedPass),
            "FunctionToLoopPassAdaptor" => fill_mangled_arg(FunctionToLoopPassAdaptor),
            "InlinerPass" => no_args(InlinerPass),
            "JumpThreadingPass" => fill_mangled_arg(JumpThreadingPass),
            "LoopFullUnrollPass" => unnamed_loop_arg(LoopFullUnrollPass),
            "PassManager<llvm::Loop, llvm::LoopAnalysisManager, llvm::LoopStandardAnalysisResults &, llvm::LPMUpdater &>" => no_args(LoopAnalysisManager),
            "GVNPass" => fill_mangled_arg(GVNPass),
            "EarlyCSEPass" => fill_mangled_arg(EarlyCSEPass),
            "MemCpyOptPass" => fill_mangled_arg(MemCpyOptPass),
            "CorrelatedValuePropagationPass" => fill_mangled_arg(CorrelatedValuePropagationPass),
            "SimplifyCFGPass" => fill_mangled_arg(SimplifyCFGPass),
            "TailCallElimPass" => fill_mangled_arg(TailCallElimPass),
            "ReassociatePass" => fill_mangled_arg(ReassociatePass),
            "LoopRotatePass" => unnamed_loop_arg(LoopRotatePass),
            "LICMPass" => unnamed_loop_arg_opt(LICMPass),
            "SCCPPass" => fill_mangled_arg(SCCPPass),
            "BDCEPass" => fill_mangled_arg(BDCEPass),
            "ADCEPass" => fill_mangled_arg(ADCEPass),
            "DSEPass" => fill_mangled_arg(DSEPass),
            "IndVarSimplifyPass" => unnamed_loop_arg(IndVarSimplifyPass),
            "PostOrderFunctionAttrsPass" => no_args(PostOrderFunctionAttrsPass),
            "LoopSimplifyPass" => fill_mangled_arg(LoopSimplifyPass),
            "LoopInstSimplifyPass" => unnamed_loop_arg(LoopInstSimplifyPass),
            "ModuleToPostOrderCGSCCPassAdaptor" => fill_path_arg(ModuleToPostOrderCGSCCPassAdaptor),
            "ModuleInlinerWrapperPass" => fill_path_arg(ModuleInlinerWrapperPass),
            "GlobalDCEPass" => fill_path_arg(GlobalDCEPass),
            "EliminateAvailableExternallyPass" => fill_path_arg(EliminateAvailableExternallyPass),
            "ReversePostOrderFunctionAttrsPass" => fill_path_arg(ReversePostOrderFunctionAttrsPass),
            "SLPVectorizerPass" => fill_mangled_arg(SLPVectorizerPass),
            "Float2IntPass" => fill_mangled_arg(Float2IntPass),
            "LoopVectorizePass" => fill_mangled_arg(LoopVectorizePass),
            "LowerConstantIntrinsicsPass" => fill_mangled_arg(LowerConstantIntrinsicsPass),
            "LoopUnrollPass" => fill_mangled_arg(LoopUnrollPass),
            "LoopSinkPass" => fill_mangled_arg(LoopSinkPass),
            "InstSimplifyPass" => fill_mangled_arg(InstSimplifyPass),
            "LoopLoadEliminationPass" => fill_mangled_arg(LoopLoadEliminationPass),
            "InjectTLIMappings" => fill_mangled_arg(InjectTLIMappings),
            "VectorCombinePass" => fill_mangled_arg(VectorCombinePass),
            "CGProfilePass" => fill_path_arg(CGProfilePass),
            "ConstantMergePass" => fill_path_arg(ConstantMergePass),
            "RelLookupTableConverterPass" => fill_path_arg(RelLookupTableConverterPass),
            "Optimizer" => no_args(Optimizer),
            _ => Err(ActivityParseError::UnknownActivity(
                name.clone(),
                args.borrow_mut().take(),
            )),
        };

        if let Err(e) = result {
            panic!("Failed to parse {name}: {e}");
        }
        result
    }

    /// Check for absence of arguments
    fn parse_empty_args(args: Option<HashMap<Box<str>, json::Value>>) -> Result<(), ArgParseError> {
        if let Some(args) = args {
            if args.is_empty() {
                Ok(())
            } else {
                Err(ArgParseError::UnexpectedKeys(args))
            }
        } else {
            Ok(())
        }
    }

    /// Parse a single "detail" string argument
    fn parse_detail_arg(
        args: Option<HashMap<Box<str>, json::Value>>,
    ) -> Result<Box<str>, ArgParseError> {
        if let Some(args) = args {
            let mut args_iter = args.into_iter();
            let collect_bad_args = |args_iter, (k, v)| {
                let mut remainder = HashMap::from_iter(args_iter);
                remainder.insert(k, v);
                remainder.remove("detail");
                remainder
            };
            if let Some((k, v)) = args_iter.next() {
                if &*k != "detail" {
                    return Err(ArgParseError::UnexpectedKeys(collect_bad_args(
                        args_iter,
                        (k, v),
                    )));
                }
                let s = if let json::Value::String(s) = v {
                    s.into()
                } else {
                    return Err(ArgParseError::UnexpectedValue("detail", v));
                };
                if let Some(kv) = args_iter.next() {
                    return Err(ArgParseError::UnexpectedKeys(collect_bad_args(
                        args_iter, kv,
                    )));
                }
                Ok(s)
            } else {
                Err(ArgParseError::MissingKey("detail"))
            }
        } else {
            Err(ArgParseError::MissingKey("detail"))
        }
    }
}

/// What an activity can take as an argument
#[derive(Clone, Debug, PartialEq)]
pub enum ActivityArgument {
    /// No argument
    Nothing,

    /// An arbitrary string
    String(Box<str>),

    /// An interned file path
    FilePath(PathKey),

    /// A C++ entity (class, function, ...)
    CppEntity(EntityKey),

    /// A C++ mangled symbol
    MangledSymbol(MangledSymbol),

    /// Either a C++ mangled symbol or nothing
    MangledSymbolOpt(Option<MangledSymbol>),
}
//
/// A mangled C++ symbol that we tried to demangle and parse
#[derive(Clone, Debug, PartialEq)]
pub enum MangledSymbol {
    /// Symbol was successfully mangled and interned
    Parsed(EntityKey),

    /// The symbol was demangled, but could not be parsed into an AST
    ///
    /// This normally happens when the demangler emits ill-formed output such
    /// as `SomeTemplate<int, && >` or `()...`. If you find reasonable output
    /// which we do not parse, please submit it as a bug.
    ///
    Demangled(Box<str>),

    /// Demangling failed and the symbol was kept in its original form.
    ///
    /// Typical patterns that fail to demangle include
    /// - __cxx_global_var_init(.<number>)?
    /// - _GLOBAL__sub_I_<source file>
    ///
    Mangled(Box<str>),
}

/// What can go wrong while parsing an Activity
#[derive(Error, Debug, PartialEq)]
pub enum ActivityParseError {
    /// Encountered an unexpected activity name
    #[error("encountered unknown activity {0:?} with arguments {1:?}")]
    UnknownActivity(Box<str>, Option<HashMap<Box<str>, json::Value>>),

    /// Failed to parse activity arguments
    #[error("failed to parse activity arguments ({0})")]
    BadArguments(#[from] ArgParseError),

    /// Encountered an unexpected activity file path
    #[error("failed to parse activity file path ({0})")]
    BadFilePath(#[from] PathError),

    /// Failed to parse a C++ entity name
    #[error("failed to parse C++ entity ({0})")]
    BadCppEntity(#[from] nom::error::Error<Box<str>>),

    /// Unexpected loop name
    #[error("unexpected loop name ({0})")]
    UnexpectedLoopName(Box<str>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ctf::{
        stack::{EndStackTrace, StackFrameId, StackTrace},
        EventCategories,
    };

    #[test]
    fn activity_stat_accessors() {
        let stat = ActivityStat {
            activity: Activity::ExecuteCompiler,
            start: 12.3,
            duration: 45.6,
        };
        assert_eq!(stat.activity(), &stat.activity);
        assert_eq!(stat.start(), stat.start);
        assert_eq!(stat.duration(), stat.duration);
        assert_eq!(stat.end(), stat.start + stat.duration);
    }

    fn test_valid_activity(
        name: &str,
        args: Option<HashMap<Box<str>, json::Value>>,
        expected: &Activity,
        expected_arg: &ActivityArgument,
        parser: &EntityParser,
    ) {
        // Check name and argument accessors
        assert_eq!(expected.name(), name);
        assert_eq!(expected.argument(), *expected_arg);

        // Check direct Activity parsing
        let name = Box::<str>::from(name);
        let mut demangling_buf = String::new();
        assert_eq!(
            Activity::parse(name.clone(), args.clone(), parser, &mut demangling_buf),
            Ok(expected.clone())
        );

        // Preparate a generator of ActivityStat inputs that are valid from
        // the Activity point of view but not from the ActivityStat point of view
        let start = 4.2;
        let duration = 1234.5;
        let make_event = |good_type, pid, tid, cat, tts, stack_trace, tdur, end_stack_trace| {
            let duration_event = DurationEvent {
                pid,
                tid,
                ts: start,
                name: Some(name.clone()),
                cat,
                tts,
                args: args.clone(),
                stack_trace,
            };
            if good_type {
                TraceEvent::X {
                    duration_event,
                    dur: duration,
                    tdur,
                    end_stack_trace,
                }
            } else {
                TraceEvent::B(duration_event)
            }
        };

        // Valid ActivityStat input
        assert_eq!(
            ActivityStat::parse(
                make_event(true, 1, 0, None, None, None, None, None),
                parser,
                &mut demangling_buf
            ),
            Ok(ActivityStat {
                activity: expected.clone(),
                start,
                duration,
            })
        );

        // Invalid inputs
        let mut test_bad_input = |input: TraceEvent| {
            assert_eq!(
                ActivityStat::parse(input.clone(), parser, &mut demangling_buf),
                Err(ActivityStatParseError::UnexpectedInput(input))
            )
        };
        test_bad_input(make_event(false, 1, 0, None, None, None, None, None));
        test_bad_input(make_event(true, 0, 0, None, None, None, None, None));
        test_bad_input(make_event(true, 1, 1, None, None, None, None, None));
        test_bad_input(make_event(
            true,
            1,
            0,
            Some(EventCategories::default()),
            None,
            None,
            None,
            None,
        ));
        test_bad_input(make_event(true, 1, 0, None, Some(start), None, None, None));
        test_bad_input(make_event(
            true,
            1,
            0,
            None,
            None,
            Some(StackTrace::sf(StackFrameId::default())),
            None,
            None,
        ));
        test_bad_input(make_event(
            true,
            1,
            0,
            None,
            None,
            None,
            Some(duration),
            None,
        ));
        test_bad_input(make_event(
            true,
            1,
            0,
            None,
            None,
            None,
            None,
            Some(EndStackTrace::esf(StackFrameId::default())),
        ));
    }

    #[test]
    fn unknown_activity() {
        let parser = EntityParser::new();
        let mut demanging_buf = String::new();
        let activity = Box::<str>::from("ThisIsMadness");
        assert_eq!(
            Activity::parse(activity.clone(), None, &parser, &mut demanging_buf),
            Err(ActivityParseError::UnknownActivity(activity))
        );
    }

    fn nullary_test(name: &str, a: Activity) {
        // Test two different ways of passing no arguments
        let parser = EntityParser::new();
        test_valid_activity(name, None, &a, &ActivityArgument::Nothing, &parser);
        test_valid_activity(
            name,
            Some(HashMap::new()),
            &a,
            &ActivityArgument::Nothing,
            &parser,
        );

        // Add an undesired detail argument
        let args = maplit::hashmap! { "detail".into() => json::json!("") };
        let mut demangling_buf = String::new();
        assert_eq!(
            Activity::parse(
                name.into(),
                Some(args.clone()),
                &parser,
                &mut demangling_buf,
            ),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedKeys(args)
            ))
        );
    }

    #[test]
    fn perform_pending_instantiations() {
        nullary_test(
            "PerformPendingInstantiations",
            Activity::PerformPendingInstantiations,
        );
    }

    #[test]
    fn frontend() {
        nullary_test("Frontend", Activity::Frontend);
    }

    #[test]
    fn per_function_passes() {
        nullary_test("PerFunctionPasses", Activity::PerFunctionPasses);
    }

    #[test]
    fn per_module_passes() {
        nullary_test("PerModulePasses", Activity::PerModulePasses);
    }

    #[test]
    fn code_gen_passes() {
        nullary_test("CodeGenPasses", Activity::CodeGenPasses);
    }

    #[test]
    fn backend() {
        nullary_test("Backend", Activity::Backend);
    }

    #[test]
    fn execute_compiler() {
        nullary_test("ExecuteCompiler", Activity::ExecuteCompiler);
    }

    fn unary_test(
        name: &str,
        arg: &str,
        expected: Activity,
        expected_argument: ActivityArgument,
        parser: EntityParser,
    ) {
        // Test happy path
        let name = Box::<str>::from(name);
        let good_args = maplit::hashmap! { "detail".into() => json::json!(arg) };
        test_valid_activity(
            &name,
            Some(good_args.clone()),
            &expected,
            &expected_argument,
            &parser,
        );

        // Try not providing the requested argument
        let mut demangling_buf = String::new();
        let missing_arg_error = Err(ActivityParseError::BadArguments(ArgParseError::MissingKey(
            "detail",
        )));
        assert_eq!(
            Activity::parse(name.clone(), None, &parser, &mut demangling_buf),
            missing_arg_error
        );
        assert_eq!(
            Activity::parse(
                name.clone(),
                Some(HashMap::new()),
                &parser,
                &mut demangling_buf
            ),
            missing_arg_error
        );

        // Try providing a wrongly typed value
        let bad_value = json::json!(42usize);
        let bad_arg_value = maplit::hashmap! { "detail".into() => bad_value.clone() };
        assert_eq!(
            Activity::parse(
                name.clone(),
                Some(bad_arg_value),
                &parser,
                &mut demangling_buf
            ),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedValue("detail", bad_value)
            ))
        );

        // Try adding a meaningless argument
        let mut bad_arg = good_args.clone();
        bad_arg.insert("wat".into(), json::json!(""));
        assert_eq!(
            Activity::parse(name, Some(bad_arg.clone()), &parser, &mut demangling_buf),
            Err(ActivityParseError::BadArguments(
                ArgParseError::UnexpectedKeys(maplit::hashmap! { "wat".into() => json::json!("") })
            ))
        );
    }

    #[test]
    fn source() {
        const PATH: &'static str =
            "/mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp";
        let parser = EntityParser::new();
        let key = parser.path_to_key(PATH);
        unary_test(
            "Source",
            PATH,
            Activity::Source(key),
            ActivityArgument::FilePath(key),
            parser,
        );
    }

    #[test]
    fn parse_class() {
        const CLASS: &'static str = "Acts::Test::MeasurementCreator";
        let parser = EntityParser::new();
        let key = parser.parse_entity(CLASS).unwrap();
        unary_test(
            "ParseClass",
            CLASS,
            Activity::ParseClass(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn instantiate_class() {
        const CLASS: &'static str = "std::invoke_result<(lambda at /mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp:354:40), Acts::detail_lt::TrackStateProxy<Acts::Test::ExtendedMinimalSourceLink, 6, 6, true> >";
        let parser = EntityParser::new();
        let key = parser.parse_entity(CLASS).unwrap();
        unary_test(
            "InstantiateClass",
            CLASS,
            Activity::InstantiateClass(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn parse_template() {
        const TEMPLATE: &'static str = "<unknown>"; // Yes, clang can do that
        let parser = EntityParser::new();
        let key = parser.parse_entity(TEMPLATE).unwrap();
        unary_test(
            "ParseTemplate",
            TEMPLATE,
            Activity::ParseTemplate(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn instantiate_function() {
        const FUNCTION: &'static str = "boost::unit_test::lazy_ostream_impl<boost::unit_test::lazy_ostream, boost::unit_test::basic_cstring<const char>, const boost::unit_test::basic_cstring<const char> &>::operator()";
        let parser = EntityParser::new();
        let key = parser.parse_entity(FUNCTION).unwrap();
        unary_test(
            "InstantiateFunction",
            FUNCTION,
            Activity::InstantiateFunction(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_type() {
        const TYPE: &'static str = "generic_dense_assignment_kernel<DstEvaluatorType, SrcEvaluatorType, Eigen::internal::add_assign_op<double, double> >";
        let parser = EntityParser::new();
        let key = parser.parse_entity(TYPE).unwrap();
        unary_test(
            "DebugType",
            TYPE,
            Activity::DebugType(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_global_variable() {
        const VAR: &'static str = "std::__detail::__variant::__gen_vtable<true, void, (lambda at /mnt/acts/Core/include/Acts/TrackFinder/CombinatorialKalmanFilter.hpp:819:11) &&, std::variant<Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime>, Acts::Measurement<Acts::Test::ExtendedMinimalSourceLink, Acts::eBoundLoc0, Acts::eBoundLoc1, Acts::eBoundPhi, Acts::eBoundTheta, Acts::eBoundQOverP, Acts::eBoundTime> > &&>::_S_vtable";
        let parser = EntityParser::new();
        let key = parser.parse_entity(VAR).unwrap();
        unary_test(
            "DebugGlobalVariable",
            VAR,
            Activity::DebugGlobalVariable(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn code_gen_function() {
        const FUNCTION: &'static str =
            "boost::unit_test::operator<<<char, std::char_traits<char>, const char>";
        let parser = EntityParser::new();
        let key = parser.parse_entity(FUNCTION).unwrap();
        unary_test(
            "CodeGen Function",
            FUNCTION,
            Activity::CodeGenFunction(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn debug_function() {
        const FUNCTION: &'static str = "Eigen::operator*<Eigen::PermutationMatrix<6, 6, int>, Eigen::CwiseNullaryOp<Eigen::internal::scalar_identity_op<double>, Eigen::Matrix<double, 6, 6, 1, 6, 6> > >";
        let parser = EntityParser::new();
        let key = parser.parse_entity(FUNCTION).unwrap();
        unary_test(
            "DebugFunction",
            FUNCTION,
            Activity::DebugFunction(key),
            ActivityArgument::CppEntity(key),
            EntityParser::new(),
        );
    }

    #[test]
    fn run_pass() {
        const PASS: &'static str = "X86 DAG->DAG Instruction Selection";
        unary_test(
            "RunPass",
            PASS,
            Activity::RunPass(PASS.into()),
            ActivityArgument::String(PASS.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn opt_function() {
        let parser = EntityParser::new();

        const VALID: &'static str = "_ZN4Acts4Test29comb_kalman_filter_zero_field11test_methodEv";
        let key = parser
            .parse_entity("Acts::Test::comb_kalman_filter_zero_field::test_method()")
            .unwrap();
        unary_test(
            "OptFunction",
            VALID,
            Activity::OptFunction(MangledSymbol::Parsed(key)),
            ActivityArgument::MangledSymbol(MangledSymbol::Parsed(key)),
            EntityParser::new(),
        );

        const INVALID: &'static str = "__cxx_global_var_init.1";
        unary_test(
            "OptFunction",
            INVALID,
            Activity::OptFunction(MangledSymbol::Mangled(INVALID.into())),
            ActivityArgument::MangledSymbol(MangledSymbol::Mangled(INVALID.into())),
            EntityParser::new(),
        );
    }

    #[test]
    fn run_loop_pass() {
        const PASS: &'static str = "Induction Variable Users";
        unary_test(
            "RunLoopPass",
            PASS,
            Activity::RunLoopPass(PASS.into()),
            ActivityArgument::String(PASS.into()),
            EntityParser::new(),
        );
    }

    #[test]
    fn opt_module() {
        const MODULE: &'static str =
            "/mnt/acts/Tests/UnitTests/Core/TrackFinder/CombinatorialKalmanFilterTests.cpp";
        let parser = EntityParser::new();
        let key = parser.path_to_key(MODULE);
        unary_test(
            "OptModule",
            MODULE,
            Activity::OptModule(key.clone()),
            ActivityArgument::FilePath(key),
            parser,
        );
    }
}
