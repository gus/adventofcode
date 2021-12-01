ROOTDIR := $(shell git rev-parse --show-toplevel)

CMDS := $(shell ls -F cmd |  grep -E '/$$' | xargs -n 1 basename | tr "\n" " ")

GO      := gotip
GOMOD   := $(GO) mod
GOBUILD := $(GO) build
GORUN   := $(GO) run
GOTEST  := $(GO) test -cover
GOBENCH  := $(GO) test -run ZZZ -bench .

GOBINPATH := $(shell go env GOPATH)/bin
GO_MODULE := $(shell cat go.mod | grep '^module' | awk '{print $$2}')

TARGET_DIR := ./targets

# installation

.PHONY: tidy install.tools

tidy:
	@echo "# running go mod tidy"
	@$(GOMOD) tidy

install.tools: tidy
	@echo "# installing tools from $(GO_MODULE)/tools"
	@$(GO) install $$($(GO) list -f '{{join .Imports " " }}' -tags tools $(GO_MODULE)/tools)

# building

.PHONY: bench $(CMDS)

$(CMDS): install.tools
	@echo "# building $@ command"
	$(GOBUILD) -o $(TARGET_DIR)/$@ ./cmd/$@

bench:
	@echo "# running all benchmarks"
	$(GOBENCH) $(GO_MODULE)/...
