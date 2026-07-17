# cics-java-liberty-springboot-link
[![Build](https://github.com/cicsdev/cics-java-liberty-springboot-link/actions/workflows/build.yaml/badge.svg)](https://github.com/cicsdev/cics-java-liberty-springboot-link/actions/workflows/build.yaml)
[![License](https://img.shields.io/badge/License-EPL%202.0-green.svg)](https://www.eclipse.org/legal/epl-2.0/)

## Overview

This sample demonstrates how to link to a Spring Boot application running in a CICS Liberty JVM server from a CICS program. It is based on a Spring MVC application that provides both a web front-end and a CICS entry point using the Link to Liberty `@CICSProgram` annotation. The sample shows how both entry points can share the same application state.

**Key Features:**
- **Link to Liberty**: Uses the `@CICSProgram` annotation to expose a Spring bean as a CICS-callable program
- **Shared Application State**: Web front-end and CICS entry point share the same in-memory message repository
- **CICS Channels and Containers**: Demonstrates passing data into a Spring Boot application via CICS channels
- **Companion COBOL Driver**: Includes a COBOL program (`GOSPRING`) to invoke the application from a CICS terminal

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Downloading](#downloading)
4. [Check Dependencies](#check-dependencies)
5. [Building the Sample](#building-the-sample)
6. [Deploying to a CICS Liberty JVM server](#deploying-to-a-cics-liberty-jvm-server)
7. [Running the Sample](#running-the-sample)
8. [Troubleshooting](#troubleshooting)
9. [License](#license)
10. [Additional Resources](#additional-resources)
11. [Contributing](#contributing)

## Prerequisites

- CICS TS V6.1 or later (required for Spring Boot 3.x and Jakarta EE 10 support)
- A configured Liberty JVM server in CICS
- Java SE 17 or later on the workstation
- An Eclipse development environment on the workstation (optional)
- Either Gradle or Apache Maven on the workstation (optional if using Wrappers)

## Downloading

**If using Eclipse:** the simplest approach is to clone the repository using the Eclipse Git plugin (EGit) perspective.

**If using the command line:**
```shell
git clone https://github.com/cicsdev/cics-java-liberty-springboot-link
```
Alternatively, download the sample as a [ZIP](https://github.com/cicsdev/cics-java-liberty-springboot-link/archive/main.zip) and unzip onto the workstation.

**If importing into Eclipse:**
1. In the **Git Repositories** view, right-click the repository → **Import as Project** (imports the root project)
   *(if you cloned from the command line, use **File → Import → Existing Projects into Workspace** instead, browse to the cloned directory, select all projects, and skip to step 6)*
2. Switch to the **Java EE** perspective
3. In the **Project Explorer**, right-click the `cics-java-liberty-springboot-link-app` folder → **Import as Project**
4. Right-click the `cics-java-liberty-springboot-link-cicsbundle` folder → **Import as Project**
5. Right-click the `cics-java-liberty-springboot-link-cicsbundle-eclipse` folder → **Import as Project**
6. **Required:** Right-click the root project → **Gradle → Refresh Gradle Project** or **Maven → Update Project...** — this resolves Spring Boot and CICS dependencies into the project classpath. Without this step, the WTP export will produce an incomplete WAR missing `WEB-INF/lib`.

### Check dependencies

Before building this sample, you should verify that the correct CICS TS bill of materials (BOM) is specified for your target release of CICS. The BOM specifies a consistent set of artifacts, and adds information about their scope. In the example below the version specified is compatible with CICS TS V6.1 with JCICS APAR PH63856, or newer. That is, the Java byte codes built by compiling against this version of JCICS will be compatible with later CICS TS versions and subsequent JCICS APARs.

You can browse the published versions of the CICS BOM at [Maven Central.](https://mvnrepository.com/artifact/com.ibm.cics/com.ibm.cics.ts.bom)

Gradle (build.gradle):

`compileOnly enforcedPlatform("com.ibm.cics:com.ibm.cics.ts.bom:6.1-20250812133513-PH63856")`

Maven (POM.xml):

```xml
<dependencyManagement>
  <dependencies>
    <dependency>
      <groupId>com.ibm.cics</groupId>
      <artifactId>com.ibm.cics.ts.bom</artifactId>
      <version>6.1-20250812133513-PH63856</version>
      <type>pom</type>
      <scope>import</scope>
    </dependency>
  </dependencies>
</dependencyManagement>
```

## Building the Sample

You can build the sample using an IDE of your choice, or you can build it from the command line. For both approaches, using the supplied Gradle or Maven wrapper is the recommended way to get a consistent version of build tooling.

On the command line, you simply swap the Gradle or Maven command for the wrapper equivalent, `gradlew` or `mvnw` respectively.

For an IDE, taking Eclipse as an example, the plug-ins for Gradle *buildship* and Maven *m2e* will integrate with the "Run As..." capability, allowing you to specify whether you want to build the project with a Wrapper, or a specific version of your chosen build tool.

The required build-tasks are `clean build` for Gradle and `clean verify` for Maven. Once run, Gradle will generate a WAR file in the `cics-java-liberty-springboot-link-app/build/libs` directory, while Maven will generate it in the `cics-java-liberty-springboot-link-app/target` directory.

**Note:** When building a WAR file for deployment to Liberty it is good practice to exclude Tomcat from the final runtime artifact. We demonstrate this in the pom.xml with the *provided* scope, and in build.gradle with the *providedRuntime()* dependency.

**Note:** If the Liberty server uses the JSF feature then the application build should add a dependency for `tiles-el`. This is required because `javax.el.ELResolver` is eligible for injection as mandated by the JSF specification, and failure to add this to the build will cause a `java.lang.NoClassDefFoundError` for `org.apache.tiles.el.ScopeELResolver` at application startup. The dependency is commented out in both `build.gradle` and `pom.xml` — uncomment it if needed.

### Gradle Wrapper (command line)

Run the following in a local command prompt:

On Linux or Mac:

```shell
./gradlew clean build
```

On Windows:

```shell
gradlew.bat clean build
```

This creates a WAR file inside the `cics-java-liberty-springboot-link-app/build/libs` directory.

> **Note:** In Eclipse, the `build` directory may be hidden by default. To view it: **Package Explorer → ⋮ → Filters and Customization → uncheck "Gradle build folder"**. For Maven, the `target` directory is visible by default.

### Maven Wrapper (command line)

Run the following in a local command prompt:

On Linux or Mac:

```shell
./mvnw clean verify
```

On Windows:

```shell
mvnw.cmd clean verify
```

This creates a WAR file inside the `cics-java-liberty-springboot-link-app/target` directory.

## Deploying to a CICS Liberty JVM server

Ensure you have the following features defined in your Liberty `server.xml`:

- `servlet-6.0` (required for Spring Boot 3.x and Jakarta EE 10)
- `cicsts:link-1.0` (required for Link to Liberty support)

A template `server.xml` is provided [here](./etc/config/liberty/server.xml).

### CICS Bundle Plugin Deployment (Gradle/Maven)

This method uses the cics-bundle-gradle-plugin or cics-bundle-maven-plugin to automatically generate a CICS bundle.

**Configure your JVM server name:**

Gradle (`cics-java-liberty-springboot-link-cicsbundle/build.gradle`):
```gradle
cics.jvmserver = 'YOUR_JVMSERVER_NAME'  // e.g., 'DFHWLP'
```

Maven (`cics-java-liberty-springboot-link-cicsbundle/pom.xml`):
```xml
<cics.jvmserver>YOUR_JVMSERVER_NAME</cics.jvmserver>  <!-- e.g., DFHWLP -->
```

**Deploy the bundle:**

1. Upload the CICS bundle ZIP file to zFS:
   - Gradle: `cics-java-liberty-springboot-link-cicsbundle/build/distributions/cics-java-liberty-springboot-link-cicsbundle-1.0.0.zip`
   - Maven: `cics-java-liberty-springboot-link-cicsbundle/target/cics-java-liberty-springboot-link-cicsbundle-1.0.0.zip`

2. Unzip the bundle on zFS

3. Create a CICS BUNDLE resource definition:
   ```
   CEDA DEFINE BUNDLE(LINKAPP) GROUP(MYGROUP) BUNDLEDIR(/path/to/bundle)
   ```

4. Install the bundle:
   ```
   CEDA INSTALL BUNDLE(LINKAPP) GROUP(MYGROUP)
   ```

**Alternative:** Use the CICS deployment API via CMCI to deploy the bundle remotely.

---

### CICS Explorer SDK Deployment

This repository includes a pre-configured Eclipse CICS bundle project `cics-java-liberty-springboot-link-cicsbundle-eclipse` that can be used directly with CICS Explorer SDK.

1. Right-click the `cics-java-liberty-springboot-link-cicsbundle-eclipse` project → **Export Bundle Project to z/OS UNIX File System** and follow the wizard

> **Note**: The bundle project is pre-configured so that the Eclipse WTP export automatically packages the application WAR with all dependencies. This relies on the `-app` project being open in the same Eclipse workspace. If you have not yet imported the project, follow steps 3 and 6 of the [Importing into Eclipse](#downloading) instructions first.

---

### Direct Liberty Application Deployment

1. Manually upload the WAR file to zFS
2. Add an `<application>` element to the Liberty server.xml to define the web application with access to all authenticated users. For example:

```xml
<application id="cics-java-liberty-springboot-link"
    location="${server.config.dir}/springapps/cics-java-liberty-springboot-link.war"
    name="cics-java-liberty-springboot-link" type="war">
    <application-bnd>
        <security-role name="cicsAllAuthenticated">
            <special-subject type="ALL_AUTHENTICATED_USERS"/>
        </security-role>
    </application-bnd>
</application>
```

> **Note:** The `id`, `location`, and `name` values use the repository name, not a versioned artifact name. This matches the context root set in `ibm-web-ext.xml` and ensures a consistent URL regardless of build tool or version.

## Running the Sample

1. Ensure the web application started successfully in Liberty by checking for msg `CWWKT0016I` in the Liberty messages.log:
   ```
   CWWKT0016I: Web application available (default_host): http://myzos.mycompany.com:httpPort/cics-java-liberty-springboot-link
   ```

2. Check for the dynamically created CICS PROGRAM resource `YOSPRING`, for example by using the Programs view in CICS Explorer.

3. Invoke the Spring Boot application from CICS using the companion COBOL driver program. Compile [GOSPRING.cbl](cobol/GOSPRING.cbl), put the load module in a suitable load library, define and install a CICS transaction with `GOSPRING` as the initial program, and start it from a terminal.

   Alternatively, invoke the application directly using CECI:
   ```
   CECI PUT CONTAINER(MESSAGE) CHAR FROM(HELLO) CHANNEL(CHAN)
   CECI LINK PROG(YOSPRING) CHANNEL(CHAN)
   ```
   Ensure you run both commands in the same CECI session.

4. After invoking the application from CICS, view the web front-end at the URL shown in step 1. You should see the message created by CICS appear in the list.

The CICS entry point for the application is in [CICSCallable.java](cics-java-liberty-springboot-link-app/src/main/java/com/ibm/cicsdev/springboot/link/app/ui/cics/CICSCallable.java). The `callMeFromCICS()` method is invoked when CICS executes `EXEC CICS LINK PROGRAM(YOSPRING)`, as specified by the `@CICSProgram` annotation.

## Troubleshooting

**Application fails to start — `CWWKZ0013E`**
- Verify `servlet-6.0` is enabled in your Liberty `server.xml`.
- Confirm CICS TS V6.1 or later is installed — earlier releases do not support Jakarta EE 10.

**CICS PROGRAM resource `YOSPRING` not created**
- Verify `cicsts:link-1.0` is enabled in your Liberty `server.xml`.
- Check the Liberty messages.log for annotation processing errors at application startup.

**`EXEC CICS LINK PROGRAM(YOSPRING)` fails with `PGMIDERR`**
- Ensure the bundle containing the WAR has been successfully installed — the PROGRAM resource is only created after Liberty loads the application.
- Verify the application started without errors (check for `CWWKT0016I`).

**Spring Boot context fails to initialise — `ClassNotFoundException` for `jakarta.*`**
- This sample uses the `jakarta.*` namespace (Spring Boot 3.x / Jakarta EE 10). Ensure you are not running on a CICS TS release older than V6.1.

## License

This project is licensed under [Eclipse Public License - v 2.0](LICENSE).

## Additional Resources

- [CICS TS Documentation — Link to Liberty](https://www.ibm.com/docs/en/cics-ts/latest?topic=liberty-link-overview)
- [CICS TS Documentation](https://www.ibm.com/docs/en/cics-ts)
- [WebSphere Liberty Documentation](https://www.ibm.com/docs/en/was-liberty)
- [Spring Boot Documentation](https://spring.io/projects/spring-boot)

## Contributing

This sample is maintained by IBM CICS development. We welcome bug reports and feature requests via GitHub Issues. Contributions are welcome and reviewed on a case-by-case basis — please read the [contributing guidelines](https://github.com/cicsdev/.github/blob/main/CONTRIBUTING.md) before opening a pull request. For CICS product questions, contact IBM Support.
