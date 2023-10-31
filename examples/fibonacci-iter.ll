; ModuleID = 'fibonacci-iter'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i64 @fib(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 1, i64* %1, align 8
  %2 = alloca i64, align 8
  store i64 1, i64* %2, align 8
  %3 = alloca i64, align 8
  store i64 1, i64* %3, align 8
  br label %while.cond_0

while.cond_0:                                     ; preds = %while.body_0, %entry_0
  %4 = load i64, i64* %3, align 8
  %5 = load i64, i64* %0, align 8
  %6 = icmp slt i64 %4, %5
  br i1 %6, label %while.body_0, label %while.exit_0

while.body_0:                                     ; preds = %while.cond_0
  %7 = load i64, i64* %1, align 8
  %8 = load i64, i64* %2, align 8
  %9 = add i64 %7, %8
  %10 = alloca i64, align 8
  store i64 %9, i64* %10, align 8
  %11 = load i64, i64* %1, align 8
  store i64 %11, i64* %2, align 8
  %12 = load i64, i64* %10, align 8
  store i64 %12, i64* %1, align 8
  %13 = load i64, i64* %3, align 8
  %14 = add i64 %13, 1
  store i64 %14, i64* %3, align 8
  br label %while.cond_0

while.exit_0:                                     ; preds = %while.cond_0
  %15 = load i64, i64* %1, align 8
  ret i64 %15
}

define i32 @main() {
entry_0:
  %0 = call i64 @fib(i64 6)
  call void @printNum(i64 %0)
  ret i32 0
}
