; ModuleID = 'project-euler6'
source_filename = "<string>"

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i64 @square-of-sum(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = load i64, i64* %0, align 8
  %2 = call i64 @sum(i64 %1)
  %3 = alloca i64, align 8
  store i64 %2, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = mul i64 %4, %5
  ret i64 %6
}

define i64 @sum(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = load i64, i64* %0, align 8
  %2 = icmp sle i64 %1, 0
  br i1 %2, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %entry_0
  %3 = load i64, i64* %0, align 8
  %4 = load i64, i64* %0, align 8
  %5 = sub i64 %4, 1
  %6 = call i64 @sum(i64 %5)
  %7 = add i64 %3, %6
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %8 = phi i64 [ 0, %if.then.ret_0 ], [ %7, %if.else.ret_0 ]
  ret i64 %8
}

define i64 @sum-of-squares(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = load i64, i64* %0, align 8
  %2 = icmp sle i64 %1, 0
  br i1 %2, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %entry_0
  %3 = load i64, i64* %0, align 8
  %4 = load i64, i64* %0, align 8
  %5 = mul i64 %3, %4
  %6 = load i64, i64* %0, align 8
  %7 = sub i64 %6, 1
  %8 = call i64 @sum-of-squares(i64 %7)
  %9 = add i64 %5, %8
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %10 = phi i64 [ 0, %if.then.ret_0 ], [ %9, %if.else.ret_0 ]
  ret i64 %10
}

define i32 @main() {
entry_0:
  %0 = alloca i64, align 8
  store i64 100, i64* %0, align 8
  %1 = load i64, i64* %0, align 8
  %2 = call i64 @square-of-sum(i64 %1)
  %3 = load i64, i64* %0, align 8
  %4 = call i64 @sum-of-squares(i64 %3)
  %5 = sub i64 %2, %4
  call void @printNum(i64 %5)
  ret i32 0
}
