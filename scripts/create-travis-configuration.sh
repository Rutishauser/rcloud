#!/bin/bash
echo "Current directory:"
rm -f conf/rcloud.conf
echo "Host: 127.0.0.1" >> conf/rcloud.conf
echo "github.client.id: ${GITHUB_CLIENT_ID}" >> conf/rcloud.conf
echo "github.client.secret: ${GITHUB_CLIENT_SECRET}" >> conf/rcloud.conf
echo "github.base.url: https://github.com/" >> conf/rcloud.conf
echo "github.api.url: https://api.github.com/" >> conf/rcloud.conf
echo "github.gist.url: https://gist.github.com/" >> conf/rcloud.conf
echo "rcs.engine: redis" >> conf/rcloud.conf
