#include<stdio.h>
#include<conio.h>

int main(){

    int user;
    printf("\nEnter the input : ");
    scanf("%d", &user);


    /*half diamond star*/
    for(int i = 0; i<user*2; i++){
        for(int j = user; j>i; j--){
            printf(" ");
        }
        for(int k = 0; k<=i; k++){
            if(i==user/2-1)
                printf("*");
            else {
                if(k>=0 && k<=i)
                    printf("*");
            }
        }

        printf("\n");
    }















    // for(int i = 0; i<user; i++){
    //     for(int j = user; j>=i; j--){
    //         printf(" ");
    //     }
    //     for(int k = 0; k<=2*i; k++){
    //         if(i==user-1)
    //             printf("*");
    //         else{
    //             if(k == 0 || k == i*2)
    //                 printf("*");
    //             else 
    //                 printf(" ");
    //         }
    //     }
    //     printf("\n");
    // }

    return 0;
}