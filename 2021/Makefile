TARGET_DIR := targets
#MODNAME := github.com/gus/adventofcode/2021
MODNAME := $(shell cat go.mod | grep '^module' | awk '{print $$2}')
CMD_PATH := "cmd"
CMD := ""

# commands/targets

CMDS := $(shell ls -d cmd/* | xargs -n 1 basename)

.PHONY: $(CMDS)

$(CMDS):
	$(eval CMD_PATH=cmd)
	$(eval CMD=$@)
	@echo "# using ${CMD}"

# building

.PHONY: require-cmd build

require-cmd:
	@if [ "X$(CMD)X" = "XX" ]; then \
		echo "! CMD required but not provided"; \
		exit 1; \
	fi
	@if [ "X$(CMD_PATH)X" = "XX" ]; then \
		echo "! CMD_PATH required but not provided"; \
		exit 1; \
	fi

build: require-cmd
	CGO_ENABLED=0 GOOS=linux go build -mod=readonly -v -o $(TARGET_DIR)/$(CMD) \
		$(MODNAME)/$(CMD_PATH)/$(CMD)

bench: require-cmd
	CGO_ENABLED=0 GOOS=linux go test -mod=readonly -bench=. $(MODNAME)/$(CMD_PATH)/$(CMD)
