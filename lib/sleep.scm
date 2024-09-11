(define (sleep seconds)
  (let* ((start-time (real-time-clock)) ; 获取当前时间
         (end-time (+ start-time (/ seconds (internal-time/ticks->seconds 1))))) ; 计算结束时间的ticks
    (let loop () ; 定义一个循环
      (if (< (real-time-clock) end-time)
          (loop))))) ; 如果当前时间小于结束时间，继续循环
