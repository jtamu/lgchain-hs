FROM haskell:9.6

ARG APP_DIR="/opt/app"

RUN mkdir -p "${APP_DIR}/.ghcup/bin"
RUN curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "${APP_DIR}/.ghcup/bin/ghcup"
RUN chmod +x "${APP_DIR}/.ghcup/bin/ghcup"

ENV PATH="${APP_DIR}/.cabal/bin:${APP_DIR}/.ghcup/bin:$PATH"

RUN ghcup install ghc 9.4.8 --set
RUN ghcup install cabal recommended --set
RUN ghcup install hls recommended --set
RUN cabal update
RUN cabal install cabal-fmt implicit-hie

WORKDIR ${APP_DIR}/src
COPY ./src/ .

# install nodejs
ENV NVM_DIR /root/.nvm
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash \
    && . "$NVM_DIR/nvm.sh" \
    && nvm install 22 \
    && nvm use 22 \
    && nvm alias default 22 \
    # install mcp servers
    && npx -y @smithery/cli install obsidian-mcp || true

ENTRYPOINT ["/bin/bash"]
