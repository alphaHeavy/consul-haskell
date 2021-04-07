INSTALL_TO := /opt/consul

install-consul: require-VERSION require-INSTALL_TO
	wget -q 'https://releases.hashicorp.com/consul/${VERSION}/consul_${VERSION}_linux_amd64.zip'
	unzip consul_${VERSION}_linux_amd64.zip
	mkdir -p ${INSTALL_TO}
	mv consul ${INSTALL_TO}/consul-${VERSION}
	chmod +x ${INSTALL_TO}/consul-${VERSION}
	echo "installed consul to ${INSTALL_TO}/consul-${VERSION}"

symlink-consul: require-VERSION require-INSTALL_TO
	sudo ln -sf  ${INSTALL_TO}/consul-${VERSION}  ${INSTALL_TO}/consul

build: require-INSTALL_TO
	PATH=${INSTALL_TO}/:${PATH} stack build --system-ghc --no-run-tests --no-run-benchmarks

test: require-INSTALL_TO
	PATH=${INSTALL_TO}/:${PATH} stack test --system-ghc --bench

require-%:
	if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi
