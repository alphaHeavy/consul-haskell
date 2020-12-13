install-consul: require-VERSION
	wget -q 'https://releases.hashicorp.com/consul/${VERSION}/consul_${VERSION}_linux_amd64.zip'
	unzip consul_${VERSION}_linux_amd64.zip
	mkdir -p ~/.local/bin/
	mv consul ~/.local/bin/consul-${VERSION}
	chmod +x ~/.local/bin/consul-${VERSION}
	echo "installed consul to ~/.local/bin/consul-${VERSION}"

symlink-consul: require-VERSION
	sudo ln -sf  ~/.local/bin/consul-${VERSION}  ~/.local/bin/consul

require-%:
	if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi
