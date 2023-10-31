; ModuleID = 'project-euler3'
source_filename = "<string>"

%OptInt = type { i8, [8 x i8] }

declare void @printNum(i64)

declare i8* @malloc(i64)

declare i32 @free(...)

define i64 @largest-prime-factor(i64 %n_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %n_0, i64* %0, align 8
  %1 = alloca %OptInt, align 8
  %2 = getelementptr %OptInt, %OptInt* %1, i32 0, i32 1
  %3 = bitcast [8 x i8]* %2 to i1*
  store i1 false, i1* %3, align 8
  %4 = getelementptr %OptInt, %OptInt* %1, i32 0, i32 0
  store i8 1, i8* %4, align 8
  %5 = load %OptInt, %OptInt* %1, align 8
  %6 = alloca %OptInt, align 8
  store %OptInt %5, %OptInt* %6, align 8
  %7 = alloca i64, align 8
  store i64 2, i64* %7, align 8
  br label %while.cond_0

while.cond_0:                                     ; preds = %if.exit_0, %entry_0
  %8 = load i64, i64* %7, align 8
  %9 = load i64, i64* %0, align 8
  %10 = sdiv i64 %9, 2
  %11 = icmp sle i64 %8, %10
  br i1 %11, label %while.body_0, label %while.exit_0

while.body_0:                                     ; preds = %while.cond_0
  %12 = load i64, i64* %0, align 8
  %13 = load i64, i64* %7, align 8
  %14 = srem i64 %12, %13
  %15 = icmp eq i64 %14, 0
  br i1 %15, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %while.body_0
  %16 = alloca %OptInt, align 8
  %17 = getelementptr %OptInt, %OptInt* %16, i32 0, i32 1
  %18 = bitcast [8 x i8]* %17 to i64*
  %19 = load i64, i64* %7, align 8
  store i64 %19, i64* %18, align 8
  %20 = getelementptr %OptInt, %OptInt* %16, i32 0, i32 0
  store i8 0, i8* %20, align 8
  %21 = load %OptInt, %OptInt* %16, align 8
  store %OptInt %21, %OptInt* %6, align 8
  %22 = load i64, i64* %0, align 8
  store i64 %22, i64* %7, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %while.body_0
  %23 = load i64, i64* %7, align 8
  %24 = add i64 %23, 1
  store i64 %24, i64* %7, align 8
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %25 = phi i1 [ false, %if.then.ret_0 ], [ false, %if.else.ret_0 ]
  br label %while.cond_0

while.exit_0:                                     ; preds = %while.cond_0
  %26 = load %OptInt, %OptInt* %6, align 8
  %27 = extractvalue %OptInt %26, 0
  %28 = icmp eq i8 0, %27
  br i1 %28, label %match.matched_0, label %match.not-matched_0

match.matched_0:                                  ; preds = %while.exit_0
  %29 = alloca [8 x i8], align 8
  %30 = extractvalue %OptInt %26, 1
  store [8 x i8] %30, [8 x i8]* %29, align 8
  %31 = bitcast [8 x i8]* %29 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = call i64 @largest-prime-factor(i64 %32)
  %34 = load i64, i64* %0, align 8
  %35 = load i64, i64* %31, align 8
  %36 = sdiv i64 %34, %35
  %37 = call i64 @largest-prime-factor(i64 %36)
  %38 = call i64 @max(i64 %33, i64 %37)
  br label %match.ret_0

match.ret_0:                                      ; preds = %match.matched_0
  br label %match.exit_0

match.not-matched_0:                              ; preds = %while.exit_0
  br label %match.matched_1

match.matched_1:                                  ; preds = %match.not-matched_0
  %39 = alloca [8 x i8], align 8
  %40 = extractvalue %OptInt %26, 1
  store [8 x i8] %40, [8 x i8]* %39, align 8
  %41 = bitcast [8 x i8]* %39 to i1*
  %42 = load i64, i64* %0, align 8
  br label %match.ret_1

match.ret_1:                                      ; preds = %match.matched_1
  br label %match.exit_0

match.exit_0:                                     ; preds = %match.ret_1, %match.ret_0
  %43 = phi i64 [ %38, %match.ret_0 ], [ %42, %match.ret_1 ]
  ret i64 %43
}

define i64 @max(i64 %a_0, i64 %b_0) {
entry_0:
  %0 = alloca i64, align 8
  store i64 %a_0, i64* %0, align 8
  %1 = alloca i64, align 8
  store i64 %b_0, i64* %1, align 8
  %2 = load i64, i64* %0, align 8
  %3 = load i64, i64* %1, align 8
  %4 = icmp sgt i64 %2, %3
  br i1 %4, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %entry_0
  %5 = load i64, i64* %0, align 8
  br label %if.then.ret_0

if.then.ret_0:                                    ; preds = %if.then_0
  br label %if.exit_0

if.else_0:                                        ; preds = %entry_0
  %6 = load i64, i64* %1, align 8
  br label %if.else.ret_0

if.else.ret_0:                                    ; preds = %if.else_0
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else.ret_0, %if.then.ret_0
  %7 = phi i64 [ %5, %if.then.ret_0 ], [ %6, %if.else.ret_0 ]
  ret i64 %7
}

define i32 @main() {
entry_0:
  %0 = call i64 @largest-prime-factor(i64 600851475143)
  call void @printNum(i64 %0)
  ret i32 0
}
