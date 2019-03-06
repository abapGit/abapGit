---
title: APACK
category: reference
order: 80
---

Packages with a class that implements `ZIF_APACK_MANIFEST` (interface is automatically created at initial start of abapGit) can define metadata that is interpreted by clients which support the APACK package and dependency management framework. Such an implementation class must reside in the top-level ABAP package which is serialized by abapGit so that it can be detected properly. After the class is instantiated, it needs to have the field `ZIF_APACK_MANIFEST~DESCRIPTOR` populated with the respective APACK metadata. The metadata follows the (https://maven.apache.org/pom.html#Maven_Coordinates)[POM format by Apache Maven] and specifies additional information as well:
- `group_id`: Unique name for your organization or project, e.g. `github.com/larshp`
- `artifact_id`: Name for the project, needs to be unique together with the specified `group_id`, e.g. `abapGit`
- `version`: Version of your project, should adhere to the (https://semver.org/)[semantic versioning concept], e.g. `1.85`
- `repository_type`: Currently fixed to `abapGit` as it's currently the only supported repository type and is set automatically during serialization
- `git_url`: The URL where your main repository is located, e.g. `https://github.com/larshp/abapGit.git`
- `dependencies`: Optional, only needs to be filled if this project has dependencies to other projects. Be sure to specify the `git_url` for each dependency properly as this is currently the only way how dependencies can automatically be resolved.

If such an implementation class is detected during the serialization process, a corresponding `.apack-mainfest.xml` file will be created on the top level of the Git repository. This contains the APACK metadata information.

### Examples

See the sample code on GitHub (uses `IF_APACK_MANIFEST` which is exclusively available on the SAP Cloud Platform ABAP Environment, descriptor format is compatible with `ZIF_APACK_MANIFEST`):
- (https://github.com/SAP/abap-platform-yy)[YY Data Service]
- (https://github.com/SAP/abap-platform-jak)[JSON ABAP Konverter] (specifies a dependency to the YY Data Service)
