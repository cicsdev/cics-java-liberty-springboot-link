# cics-java-liberty-springboot-link
[![Build](https://github.com/cicsdev/cics-java-liberty-springboot-link/actions/workflows/build.yaml/badge.svg)](https://github.com/cicsdev/cics-java-liberty-springboot-link/actions/workflows/build.yaml)
[![License](https://img.shields.io/badge/License-EPL%202.0-green.svg)](https://opensource.org/licenses/EPL-2.0)

## Overview

This sample demonstrates how to link to a Spring Boot application running in a CICS Liberty JVM server from a CICS program. It is based on a Spring MVC application that provides both a web front-end and a CICS entry point using the Link to Liberty `@CICSProgram` annotation. The sample shows how both entry points can share the same application state.

**Key Features:**
- Demonstrates the CICS Link to Liberty feature using the `@CICSProgram` annotation
- Provides a Spring MVC web front-end and a CICS-callable entry point in a single application
- Shows how CICS channels and containers can be used to pass data into a Spring Boot application
- Includes a companion COBOL driver program to invoke the application from a CICS terminal

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Downloading](#downloading)
4. [Check Dependencies](#check-dependencies)
5. [Building the Sample](#building-the-sample)
6. [Deploying to a CICS Liberty JVM server](#deploying-to-a-cics-liberty-jvm-server)
7. [Running the Sample](#running-the-sample)
8. [License](#license)
9. [Additional Resources](#additional-resources)
10. [Contributing](#contributing)

## Prerequisites

- CICS TS V6.1 or later
- A configured Liberty JVM server in CICS
- Java SE 17 or later on the workstation
- An Eclipse development environment on the workstation (optional)
- Either Gradle or Apache Maven on the workstation (optional if using Wrappers)

## Downloading

- Clone the repository using your IDE's support, such as the Eclipse Git plugin
- **or**, download the sample as a [ZIP](https://github.com/cicsdev/cics-java-liberty-springboot-link/archive/main.zip) and unzip onto the workstation

>*Tip: Eclipse Git provides an 'Import existing Projects' check-box when cloning a repository.*

### Check dependencies

Before building this sample, you should verify that the correct CICS TS bill of materials (BOM) is specified for your target release of CICS. The BOM specifies a consistent set of artifacts and adds information about their scope. In the example below the version specified is compatible with CICS TS V6.1 with JCICS APAR PH63856, or newer. That is, the Java byte codes built by compiling against this version of JCICS will be compatible with later CICS TS versions and subsequent JCICS APARs.

You can browse the published versions of the CICS BOM at [Maven Central](https://mvnrepository.com/artifact/com.ibm.cics/com.ibm.cics.ts.bom).

Gradle (build.gradle):

`compileOnly enforcedPlatform("com.ibm.cics:com.ibm.cics.ts.bom:6.1-20250812133513-PH63856")`

Maven (pom.xml):

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

**Note:** If you import the project to your IDE, you might experience local project compile errors. To resolve these errors you should run a tooling refresh on that project. For example, in Eclipse: right-click on "Project", select "Gradle → Refresh Gradle Project", **or** right-click on "Project", select "Maven → Update Project...".

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

## Deploying to a CICS Liberty JVM server

Ensure you have the following features defined in your Liberty `server.xml`:

- `cicsts:link-1.0`
- `cicsts:security-1.0` if CICS security is enabled

A template `server.xml` is provided [here](./etc/config/liberty/server.xml).

### CICS Bundle Plugin Deployment (Gradle/Maven)

This is the **recommended** deployment method as it uses the CICS bundle generated during the build process.

1. Upload the CICS bundle ZIP file to zFS:
   - Gradle: `cics-java-liberty-springboot-link-cicsbundle/build/distributions/`
   - Maven: `cics-java-liberty-springboot-link-cicsbundle/target/`

2. Unzip the bundle on zFS, then create and install a CICS BUNDLE resource definition pointing to the unzipped directory.

### CICS Explorer SDK Deployment

1. Copy and paste the built WAR from your `target` or `build/libs` directory into an Eclipse CICS Bundle project and create a new WAR bundlepart that references the WAR file.
2. Deploy the CICS Bundle project from CICS Explorer using the **Export Bundle Project to z/OS UNIX File System** wizard.

### Direct Liberty Application Deployment

1. Manually upload the WAR file to zFS.
2. Add an `<application>` element to the Liberty `server.xml` to define the web application with access to all authenticated users:

```xml
<application id="cics-java-liberty-springboot-link-app-1.0.0"
    location="${server.config.dir}/springapps/cics-java-liberty-springboot-link-app-1.0.0.war"
    name="cics-java-liberty-springboot-link-app-1.0.0" type="war">
    <application-bnd>
        <security-role name="cicsAllAuthenticated">
            <special-subject type="ALL_AUTHENTICATED_USERS"/>
        </security-role>
    </application-bnd>
</application>
```

## Running the Sample

1. Ensure the web application started successfully in Liberty by checking for msg `CWWKT0016I` in the Liberty messages.log:

   `A CWWKT0016I: Web application available (default_host): http://myzos.mycompany.com:httpPort/cics-java-liberty-springboot-link-app-1.0.0`

2. Check for the dynamically created CICS PROGRAM resource `YOSPRING`, for example by using the Programs view in CICS Explorer.

3. Invoke the Spring Boot application from CICS. You can use the COBOL program provided in this repository — compile [GOSPRING.cbl](cobol/GOSPRING.cbl), put the load module in a suitable load library, define and install a CICS transaction with `GOSPRING` as the initial program, and start it from a terminal.

   Alternatively, use CECI:

   ```
   CECI PUT CONTAINER(MESSAGE) CHAR FROM(HELLO) CHANNEL(CHAN)
   CECI LINK PROG(YOSPRING) CHANNEL(CHAN)
   ```

   Ensure you run both commands in the same CECI session.

4. After invoking the application from CICS, view the web front-end at the URL shown in `messages.log`. You should see the message created by CICS.

The CICS entry point for the application is in [CICSCallable.java](cics-java-liberty-springboot-link-app/src/main/java/com/ibm/cicsdev/springboot/link/app/ui/cics/CICSCallable.java). The `callMeFromCICS()` method is invoked when CICS executes `EXEC CICS LINK PROGRAM(YOSPRING)`, as specified by the `@CICSProgram` annotation.

## License

This project is licensed under [Eclipse Public License - v 2.0](LICENSE).

## Additional Resources

- [CICS TS Documentation — Link to Liberty](https://www.ibm.com/docs/en/cics-ts/latest?topic=liberty-link-overview)
- [CICS TS Documentation](https://www.ibm.com/docs/en/cics-ts)
- [WebSphere Liberty Documentation](https://www.ibm.com/docs/en/was-liberty)
- [Spring Boot Documentation](https://spring.io/projects/spring-boot)
- [CICS Java samples on GitHub](https://github.com/cicsdev)

## Contributing

We welcome bug reports and feedback! Please see [CONTRIBUTING.md](https://github.com/cicsdev/.github/blob/main/CONTRIBUTING.md) for guidelines on reporting issues.
