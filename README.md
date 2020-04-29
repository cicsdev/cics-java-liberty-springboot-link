# cics-java-liberty-springboot-link

This is a sample for Link to Liberty, demonstrating how you can invoke a Spring Boot application in a Liberty JVM server from a CICS program. The application is based on a Spring Boot sample application that demonstrates Spring MVC, so has both a web front-end and a CICS entry point. The sample shows how both entry points can access the same data.

## Prerequisites

  - CICS TS V5.5 with APAR PH14856, or later
  - A configured Liberty JVM server 
  - Java SE 1.8 or later on the z/OS system
  - Java SE 1.8 or later on the workstation
  
## Building 

You can choose to build the project using Gradle or Maven. The project includes both Gradle and Maven wrappers, these wrappers will automatically download required components from your chosen build tool; if not already present on your workstation.

You can also build the sample project through plug-in tooling of your chosen IDE. Both Gradle *buildship* and Maven *m2e* will integrate with Eclipse's "Run As..." capability allowing you to specify the required build-tasks. There are typically `clean bootWar` for Gradle and `clean package` for Maven, as reflected in the command line approach shown later.

**Note:** When building a WAR file for deployment to Liberty it is good practice to exclude Tomcat from the final runtime artifact. We demonstrate this in the pom.xml with the *provided* scope, and in build.gradle with the *providedRuntime()* dependency.

**Note:** If you import the project to an IDE of your choice, you might experience local project compile errors. To resolve these errors you should refresh your IDEs configuration. For example, in Eclipse: for Gradle, right-click on "Project", select "Gradle -> Refresh Gradle Project", or for Maven, right-click on "Project", select "Maven -> Update Project...".

### Gradle

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

### Maven

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

    
## Deploying the sample
1. Ensure you have the following features in `server.xml`: 

    - *cicsts:link-1.0*
    - *cicsts:security-1.0* 

2. Copy and paste the WAR from your *target* or *build/libs* directory into a CICS bundle project and create a new WARbundlepart for that WAR file. 

3. Deploy the CICS bundle project as normal. For example in Eclipse, select "Export Bundle Project to z/OS UNIX File System".

4. Optionally, manually upload the WAR file to zFS and add an `<application>` configuration to server.xml:

``` XML
   <application id="com.ibm.cicsdev.springboot.link-0.1.0"  
     location="${server.config.dir}/springapps/com.ibm.cicsdev.springboot.link-0.1.0.war"  
     name="com.ibm.cicsdev.springboot.link-0.1.0" type="war">
     <application-bnd>
        <security-role name="cicsAllAuthenticated">
            <special-subject type="ALL_AUTHENTICATED_USERS"/>
        </security-role>
     </application-bnd>  
   </application>
```
5. Check for the dynamically created CICS PROGRAM resource YOSPRING for example by using the Programs view in CICS Explorer.

## Trying out the sample
You can use the COBOL program provided in this repository to invoke the Spring Boot application.  Compile [GOSPRING.cbl](cobol/GOSPRING.cbl) and put the load module in a suitable load library.  Define and install a CICS transaction with GOSPRING as the initial program, and start it from a terminal. 

Alternatively, you can use CECI to invoke the sample program:

```
CECI PUT CONTAINER(MESSAGE) CHAR FROM(HELLO) CHANNEL(CHAN)
CECI LINK PROG(YOSPRING) CHANNEL(CHAN)
```
Ensure you run both commands in the same CECI seesion.

After invoking the Spring Boot applicaiton from CICS, you can view its web front end. Find the URL the application is published on in `messages.log` e.g. `http://myzos.mycompany.com:32000/springboot-link-app-1.0.0/`. The browser will prompt for basic authentication. Enter a valid userid and password - according to the configured registry for your target Liberty JVM server.  You should be able to see a message created by CICS!

## Examining the code

The CICS entry point for the application is in [CICSCallable.java](src/main/java/com/ibm/cicsdev/springboot/link/app/ui/cics/CICSCallable.java).  This class is a Spring Bean, annotated with `@Component`.  It has an autowired referenced to a `MessageRespository`, which the Spring Framework will inject.

`callMeFromCICS()` is the method that CICS will invoke when you perform an `EXEC CICS LINK PROGRAM(YOSPRING)`.  This is specified by the `@CICSProgram` annotation.  The code obtains the current channel and gets a container in order to obtain the data passed by the calling COBOL program.  It then creates a message in the `MessageRespoitory`. 

## License
This project is licensed under [Apache License Version 2.0](LICENSE). 
