FROM haskell:8.8.4-buster

RUN echo "deb [trusted=yes] http://apt.llvm.org/buster/ llvm-toolchain-buster-9 main" >> /etc/apt/sources.list.d/llvm.list && \
    echo "deb-src [trusted=yes] http://apt.llvm.org/buster/ llvm-toolchain-buster-9 main" >> /etc/apt/sources.list.d/llvm.list && \
    apt-get update && \
    apt-get install llvm-9-dev clang-9 -y && \
    ln -s /usr/bin/clang-9 /usr/bin/clang && \
    cabal update
COPY . /mail
WORKDIR /mail
RUN cabal install mail
CMD ["/bin/bash"]
