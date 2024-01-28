#! /usr/bin/env bash

go install -v honnef.co/go/tools/cmd/staticcheck@latest
go install -v github.com/davidrjenni/reftools/cmd/fillstruct@latest
go install -v golang.org/x/tools/gopls@latest
go install -v golang.org/x/tools/cmd/goimports@latest
go install -v github.com/goreleaser/goreleaser@latest

