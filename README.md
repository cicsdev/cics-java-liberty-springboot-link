# cics-java-liberty-springboot-link

This sample demonstrates how you can link to a Spring Boot application in a Liberty JVM server from a CICS program. The application is based on a Spring Boot sample application that demonstrates Spring MVC, so has both a web front-end and a CICS entry point using the link to Liberty `@CICSProgram` annotation. The sample shows how both entry points can access the same data.

## Versions
| CICS TS for z/OS Version | Branch                                 | Minimum Java Version | Build Status |
|--------------------------|----------------------------------------|----------------------|--------------|
| 5.5, 5.6, 6.1            | [cicsts/v5.5](/../../tree/cicsts/v5.5) | 8                    | [![Build](https://github.com/cicsdev/cics-java-osgi-linkactions/workflows/java.yaml/badge.svg?branch=cicsts%2Fv5.5)](https://github.com/cicsdev/cics-java-osgi-link/actions/workflows/java.yaml) |

## License
This project is licensed under [Eclipse Public License - v 2.0](LICENSE).
