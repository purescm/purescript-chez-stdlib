(library (Chez.Thread foreign)
  (export
    forkThreadImpl
    joinThreadImpl
    makeMutex
    withMutexImpl
    mutexNameImpl
    makeCondition
    conditionWaitImpl
    conditionSignalImpl
    sleepImpl)
  (import (chezscheme))

  (define forkThreadImpl
    (lambda (threadFn)
      (fork-thread
        (lambda ()
          (threadFn (get-thread-id))))))

  (define joinThreadImpl (lambda (thread) (thread-join thread)))

  (define makeMutex (lambda () (make-mutex)))

  (define withMutexImpl (lambda (mutex thunk) (with-mutex mutex (thunk))))

  (define mutexNameImpl (lambda (mutex) (mutex-name mutex)))

  (define makeCondition (lambda () (make-condition)))

  (define conditionWaitImpl (lambda (condition mutex) (condition-wait condition mutex)))

  (define conditionSignalImpl (lambda (condition) (condition-signal condition)))

  ;; TODO: make-time shoud probably be in a Chez.Time module
  (define sleepImpl (lambda (nanoseconds seconds) (sleep (make-time 'time-duration nanoseconds seconds))))
  )
