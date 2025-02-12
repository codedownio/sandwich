#!/usr/bin/env sh

stack build --stack-yaml stack.yaml
stack build --stack-yaml stack-9.2.8.yaml
stack build --stack-yaml stack-9.4.8.yaml
stack build --stack-yaml stack-9.6.6.yaml
stack build --stack-yaml stack-9.8.4.yaml
stack build --stack-yaml stack-9.10.1.yaml
