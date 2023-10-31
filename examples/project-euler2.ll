; ModuleID = 'project-euler2'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i32 @main() {
entry_0:
  %0 = alloca i64, align 8
  store i64 0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 1, i64* %1, align 8
  %2 = alloca i64, align 8
  store i64 1, i64* %2, align 8
  br label %while.cond_0

while.cond_0:                                     ; preds = %if.exit_0, %entry_0
  %3 = load i64, i64* %1, align 8
  %4 = icmp sle i64 %3, 4000000
  br i1 %4, label %while.body_0, label %while.exit_0

while.body_0:                                     ; preds = %while.cond_0
  %5 = load i64, i64* %1, align 8
  %6 = srem i64 %5, 2
  %7 = icmp eq i64 %6, 0
  br i1 %7, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %while.body_0
  %8 = load i64, i64* %0, align 8
  %9 = load i64, i64* %1, align 8
  %10 = add i64 %8, %9
  store i64 %10, i64* %0, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %while.body_0
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %11 = phi i1 [ false, %if.then.ret_0 ], [ false, %if.else.ret_0 ]
  %12 = load i64, i64* %1, align 8
  %13 = load i64, i64* %2, align 8
  %14 = add i64 %12, %13
  %15 = alloca i64, align 8
  store i64 %14, i64* %15, align 8
  %16 = load i64, i64* %1, align 8
  store i64 %16, i64* %2, align 8
  %17 = load i64, i64* %15, align 8
  store i64 %17, i64* %1, align 8
  br label %while.cond_0

while.exit_0:                                     ; preds = %while.cond_0
  %18 = load i64, i64* %0, align 8
  call void @printNum(i64 %18)
  ret i32 0
}
