#!/usr/bin/env python3

# Make executable: chmod +x praat_consonance.py

import parselmouth
import joblib
import click
import contextlib
import statistics
import json

from tqdm import tqdm
from joblib import Parallel, delayed

@contextlib.contextmanager
def tqdm_joblib(tqdm_object):
    # Credit: https://stackoverflow.com/questions/24983493/tracking-progress-of-joblib-parallel-execution
    """Context manager to patch joblib to report into tqdm progress bar given as argument"""
    class TqdmBatchCompletionCallback(joblib.parallel.BatchCompletionCallBack):
        def __call__(self, *args, **kwargs):
            tqdm_object.update(n=self.batch_size)
            return super().__call__(*args, **kwargs)

    old_batch_callback = joblib.parallel.BatchCompletionCallBack
    joblib.parallel.BatchCompletionCallBack = TqdmBatchCompletionCallback
    try:
        yield tqdm_object
    finally:
        joblib.parallel.BatchCompletionCallBack = old_batch_callback
        tqdm_object.close() 
        
def analyse_chords(input_, output, maxproc):
    with open(input_) as f:
        chords = json.load(f)
    n_chord = len(chords)
    n_proc = min(n_chord, maxproc)
    # with tqdm_joblib(tqdm(desc="... analysing with Praat", total=len(chords))) as progress_bar:
    res = Parallel(n_jobs=n_proc)(delayed(analyse_chord)(chord) for chord in chords)
    with open(output, "w") as f:
        json.dump(res, f)

def analyse_chord(chord):
    file = chord["file"]
    f0_ceiling = chord["f0_ceiling"]
    sound = parselmouth.Sound(file)
    harmonicity = sound.to_harmonicity(time_step=0.1).values.tolist()[0]
    harmonicity = [x for x in harmonicity if x > -199]
    harmonicity = statistics.median(harmonicity)
    
    f0 = sound.to_pitch(
        time_step=0.1, 
        pitch_floor=10, 
        pitch_ceiling=f0_ceiling
    ).to_matrix().values.tolist()[0]
    f0 = statistics.median(f0)
    
    return {
        "harmonicity": harmonicity,
        "f0": f0
    }
