//! Processing thread of the TUI interface (owns the ClangTrace and takes care
//! of all the expensive rendering operations to allow good responsiveness).

use clang_time_trace::{ClangTrace, ClangTraceLoadError};
use std::{
    path::Path,
    sync::{
        mpsc::{self, Receiver, Sender},
        Arc, Mutex, TryLockError,
    },
    thread::{self, JoinHandle},
    time::Duration,
};

/// Encapsulation of the processing thread
pub struct ProcessingThread<Metadata: Send + Sync + 'static> {
    /// JoinHandle of the processing thread
    handle: JoinHandle<()>,

    /// ClangTrace metadata (if already available and not yet fetched)
    metadata: Arc<Mutex<Option<Result<Metadata, ClangTraceLoadError>>>>,

    /// Channel to send instructions to the processing thread
    instruction_sender: Sender<Instruction>,
}
//
impl<Metadata: Send + Sync + 'static> ProcessingThread<Metadata> {
    /// Set up the processing thread, indicating how metadata should be produced
    pub fn new(
        path: &Path,
        generate_metadata: impl Send + FnOnce(&ClangTrace) -> Metadata + 'static,
    ) -> Self {
        // Set up processing thread state and communication channels
        let path = path.to_path_buf();
        let metadata = Arc::new(Mutex::new(None));
        let metadata2 = metadata.clone();
        let (instruction_sender, instruction_receiver) = mpsc::channel();

        // Spawn the processing thread
        let handle = thread::spawn(move || {
            // Load the ClangTrace and generate user-specified metadata
            let trace = {
                let mut metadata_lock = metadata2.lock().expect("Main thread has crashed");
                match ClangTrace::from_file(path) {
                    Ok(trace) => {
                        *metadata_lock = Some(Ok(generate_metadata(&trace)));
                        trace
                    }
                    Err(e) => {
                        *metadata_lock = Some(Err(e));
                        panic!("Failed to load ClangTrace")
                    }
                }
            };

            // Process instructions until the main thread hangs up
            worker(trace, instruction_receiver);
        });

        // Wait for the thread to start processing the ClangTrace (as indicated
        // by acquisition of the metadata lock or insertion of metadata)
        while matches!(
            *metadata.try_lock().expect("Processing thread has crashed"),
            None
        ) {
            std::thread::sleep(Duration::new(0, 1_000));
        }
        Self {
            handle,
            metadata,
            instruction_sender,
        }
    }

    /// Extract the previously requested ClangTrace metadata, if available
    ///
    /// May return...
    /// - None if the ClangTrace is still being processed
    /// - Some(Ok(metadata)) if the processing thread is ready
    /// - Some(Err(error)) if the processing thread failed to load the ClangTrace
    ///
    /// Will panic if the processing thread has panicked or the ClangTrace
    /// metadata has already been extracted through a call to this function
    ///
    pub fn try_extract_metadata(&mut self) -> Option<Result<Metadata, ClangTraceLoadError>> {
        match self.metadata.try_lock() {
            Ok(mut guard) => match guard.take() {
                opt @ Some(_) => opt,
                None => panic!("Metadata has already been extracted"),
            },
            Err(TryLockError::WouldBlock) => None,
            Err(TryLockError::Poisoned(e)) => {
                panic!("Processing thread has crashed: {e}")
            }
        }
    }
}

/// Processing thread worker
fn worker(trace: ClangTrace, instructions: Receiver<Instruction>) {
    while let Ok(instruction) = instructions.recv() {
        match instruction {
            () => {}
        }
    }
}

// TODO: Here be an enum of possible instructions
type Instruction = ();
