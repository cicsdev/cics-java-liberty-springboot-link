# cics-java-liberty-springboot-link

This sample demonstrates how you can link to a Spring Boot application in a Liberty JVM server from a CICS program. The application is based on a Spring Boot sample application that demonstrates Spring MVC, so has both a web front-end and a CICS entry point using the link to Liberty `@CICSProgram` annotation. The sample shows how both entry points can access the same data.

## Prerequisites

  - CICS TS V5.5 with APAR PH14856, or later
  - A configured Liberty JVM server in CICS
  - Java SE 1.8 on the z/OS system
  - Java SE 1.8 on the workstation
  - An Eclipse development environment on the workstation (optional)
  - Either Gradle or Apache Maven on the workstation (optional if using Wrappers)
  

## Downloading

- Clone the repository using your IDEs support, such as the Eclipse Git plugin
- **or**, download the sample as a [ZIP](https://github.com/cicsdev/cics-java-liberty-springboot-link/archive/master.zip) and unzip onto the workstation

>*Tip: Eclipse Git provides an 'Import existing Projects' check-box when cloning a repository.*


### Check dependencies
 
Before building this sample, you should verify that the correct CICS TS bill of materials (BOM) is specified for your target release of CICS. The BOM specifies a consistent set of artifacts, and adds information about their scope. In the example below the version specified is compatible with CICS TS V5.5 with JCICS APAR PH25409, or newer. That is, the Java byte codes built by compiling against this version of JCICS will be compatible with later CICS TS versions and subsequent JCICS APARs. 
You can browse the published versions of the CICS BOM at [Maven Central.](https://mvnrepository.com/artifact/com.ibm.cics/com.ibm.cics.ts.bom)
 
Gradle (build.gradle): 

`compileOnly enforcedPlatform("com.ibm.cics:com.ibm.cics.ts.bom:5.5-20200519131930-PH25409")`

Maven (POM.xml):

``` xml	
<dependencyManagement>
    <dependencies>
      <dependency>
        <groupId>com.ibm.cics</groupId>
        <artifactId>com.ibm.cics.ts.bom</artifactId>
        <version>5.5-20200519131930-PH25409</version>
        <type>pom</type>
        <scope>import</scope>
      </dependency>
    </dependencies>
  </dependencyManagement>
  ```


## Building 

You can build the sample using an IDE of your choice, or you can build it from the command line. For both approaches, using the supplied Gradle or Maven wrapper is the recommended way to get a consistent version of build tooling. 

On the command line, you simply swap the Gradle or Maven command for the wrapper equivalent, `gradlew` or `mvnw` respectively.
  
For an IDE, taking Eclipse as an example, the plug-ins for Gradle *buildship* and Maven *m2e* will integrate with the "Run As..." capability, allowing you to specify whether you want to build the project with a Wrapper, or a specific version of your chosen build tool.

The required build-tasks are typically `clean bootWar` for Gradle and `clean package` for Maven. Once run, Gradle will generate a WAR file in the `build/libs` directory, while Maven will generate it in the `target` directory.

**Note:** When building a WAR file for deployment to Liberty it is good practice to exclude Tomcat from the final runtime artifact. We demonstrate this in the pom.xml with the *provided* scope, and in build.gradle with the *providedRuntime()* dependency.

**Note:** If you import the project to your IDE, you might experience local project compile errors. To resolve these errors you should run a tooling refresh on that project. For example, in Eclipse: right-click on "Project", select "Gradle -> Refresh Gradle Project", **or** right-click on "Project", select "Maven -> Update Project...".

>Tip: *In Eclipse, Gradle (buildship) is able to fully refresh and resolve the local classpath even if the project was previously updated by Maven. However, Maven (m2e) does not currently reciprocate that capability. If you previously refreshed the project with Gradle, you'll need to manually remove the 'Project Dependencies' entry on the Java build-path of your Project Properties to avoid duplication errors when performing a Maven Project Update.*  

#### Gradle Wrapper (command line)

Run the following in a local command prompt:

On Linux or Mac:

```shell
./gradlew clean bootWar
```
On Windows:

```shell
gradlew.bat clean bootWar
```

This creates a WAR file inside the `build/libs` directory.

#### Maven Wrapper (command line)


Run the following in a local command prompt:

On Linux or Mac:

```shell
./mvnw clean package
```

On Windows:

```shell
mvnw.cmd clean package
```

This creates a WAR file inside the `target` directory.


## Deploying to a CICS Liberty JVM server

- Ensure you have the following features defined in your Liberty `server.xml`:           
    - `<cicsts:link-1.0>`. 
    - `<cicsts:security-1.0>` if CICS security is enabled.
    
- Deployment option 1:
    - Copy and paste the built WAR from your *target* or *build/libs* directory into a Eclipse CICS bundle project and create a new WAR bundlepart that references the WAR file. Then deploy the CICS bundle project from CICS Explorer using the **Export Bundle Project to z/OS UNIX File System** wizard.
    
   
- Deployment option 2:
    - Manually upload the WAR file to zFS and add an `<application>` element to the Liberty server.xml to define the web application with access to all authenticated users. For example the following application element can be used to install a WAR, and grant access to all authenticated users if security is enabled.
 
``` XML
<application id="cics-java-liberty-springboot-link-0.1.0"  
    location="${server.config.dir}/springapps/cics-java-liberty-springboot-link-0.1.0.war"  
    name="cics-java-liberty-springboot-link-0.1.0" type="war">
    <application-bnd>
        <security-role name="cicsAllAuthenticated">
        <special-subject type="ALL_AUTHENTICATED_USERS"/>
    </security-role>
   </application-bnd>  
</application>
```

- Check for the dynamically created CICS PROGRAM resource YOSPRING for example by using the Programs view in CICS Explorer.

## Trying out the sample

1. Ensure the web application started successfully in Liberty by checking for msg `CWWKT0016I` in the Liberty messages.log:
   
    - `A CWWKT0016I: Web application available (default_host): http://myzos.mycompany.com:httpPort/cics-java-liberty-springboot-link-0.1.0`
    - `I SRVE0292I: Servlet Message - [cics-java-liberty-springboot.link-0.1.0]:.Initializing Spring embedded WebApplicationContext`

2. You can use the COBOL program provided in this repository to invoke the Spring Boot application.  Compile [GOSPRING.cbl](cobol/GOSPRING.cbl) and put the load module in a suitable load library.  Define and install a CICS transaction with GOSPRING as the initial program, and start it from a terminal. 

Alternatively, you can use CECI to invoke the sample program:

```
CECI PUT CONTAINER(MESSAGE) CHAR FROM(HELLO) CHANNEL(CHAN)
CECI LINK PROG(YOSPRING) CHANNEL(CHAN)
```
Ensure you run both commands in the same CECI seesion.

3. After invoking the Spring Boot applicaiton from CICS, you can view its web front end. Find the URL the application is published on in `messages.log` e.g. `http://myzos.mycompany.com:32000/cics-java-liberty-springboot-link-0.1.0/`. You should be able to see a message created by CICS!

## Examining the code

The CICS entry point for the application is in [CICSCallable.java](src/main/java/com/ibm/cicsdev/springboot/link/app/ui/cics/CICSCallable.java).  This class is a Spring Bean, annotated with `@Component`.  It has an autowired referenced to a `MessageRespository`, which the Spring Framework will inject.

`callMeFromCICS()` is the method that CICS will invoke when you perform an `EXEC CICS LINK PROGRAM(YOSPRING)`.  This is specified by the `@CICSProgram` annotation.  The code obtains the current channel and gets a container in order to obtain the data passed by the calling COBOL program.  It then creates a message in the `MessageRespoitory`. 

## License
This project is licensed under [Eclipse Public License - v 2.0](LICENSE). 
