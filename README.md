# CTS Worklist - OData service and ABAP backend code

A UI5 app for working with transports and objects. This repository contains the  OData service and ABAP code for the application. For the UI5 application please see https://github.com/blahbap/cts-worklist/

## Installation 
Install using abapGit into package $CTSWORKLIST

## Activate the following services in SICF 

1. /sap/public/info
2. /sap/bc/ui2/start_up

## Activate OData service
In transaction `/n/iwfnd/maint_service`, find the service `ZCTSW_TRANSPORT_SRV` and create a LOCAL alias for the service:  


![image](https://github.com/blahbap/sap-ca-cts-worklist-abap/assets/323613/23f7cdc1-48c2-4ed0-a132-39a85e7e69e7)

![image](https://github.com/blahbap/sap-ca-cts-worklist-abap/assets/323613/574d335d-1475-45ee-a5a9-f8616e251ecf)

