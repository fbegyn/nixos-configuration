#! /usr/bin/env bash

go install honnef.co/go/tools/cmd/staticcheck@latest
go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/goreleaser/goreleaser@latest

