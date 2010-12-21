/* dummy provider for testing failed driver initialization */
#include <xsldrv.h>

DriverState init_provider() {
    return( ProviderError );
}

void handle_request(void* response) {
    /*this just keeps the compiler happy*/
}

DriverState post_handle_request(void* response) {
    /*this just keeps the compiler happy*/
    return( Ok );
}

/*
 * Gives the implementation provider a change to cleanup.
 */
void destroy_provider() {}
