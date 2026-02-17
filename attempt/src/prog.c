
#include <stdio.h>
#include <wstp.h>

int main()
{

    WSENV ep;

    ep = WSInitialize((WSEnvironmentParameter)0);
    if (ep == (WSENV)0)
    { 
        printf("Unable to initialize WSTP environment.\n");
    }


    WSDeinitialize(ep);

    return 0;
}