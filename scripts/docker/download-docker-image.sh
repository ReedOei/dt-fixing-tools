if ! [ -x "$(command -v azcopy)" ]; then
    echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod/ xenial main" > azure.list
    sudo cp -f ./azure.list /etc/apt/sources.list.d/
    sudo apt-key adv --keyserver packages.microsoft.com --recv-keys EB3E94ADBE1229CF
    sudo apt-get update
    sudo apt-get install -y apt-transport-https
    sudo apt-get update
    sudo apt-get install -y azcopy
fi

line=$(cat $1)
imageName=$(echo ${line} | cut -d',' -f5)

azcopy \
    --source https://dtdataset.file.core.windows.net/dtdata/$imageName \
    --destination ./$imageName \
    --source-key $2 \
    --preserve-last-modified-time \
    --quiet

docker load -i $imageName
