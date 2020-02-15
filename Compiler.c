#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include<ctype.h>

#define NUMBER_Func 100
#define NUMBER_VARS 100
#define taille_tab 20
#define taille_tab_sym 20

typedef enum {NUM, STR, VECT, BBOOL} TSYM;
char toshowType[4][20] = {"NUMERIC", "STRING", "VECTOR", "BBOOL"};
typedef  struct {char* NOM[50]; TSYM TIDF;} T_TAB_IDF;
T_TAB_IDF TAB_IDFS[NUMBER_VARS];

typedef enum{ADD,SUB,MUL,DIV,EQL,NEQ,GTR,LSS,GEQ,LEQ,PRN,INN,INT,LDI,LDA,LDV,STO,BRN,BZE,HLT} MNEMONIQUES;

char* tabenum[20] = {"ADD","SUB","MUL","DIV","EQL","NEQ","GTR","LSS","GEQ","LEQ","PRN","INN","INT","LDI","LDA","LDV","STO","BRN","BZE","HLT"};

typedef struct{MNEMONIQUES MNE; int SUITE;}Instruction;


typedef struct { char* CODE  ; char NOM[20]; int val;  } TSym_Cour;
TSym_Cour   SYM_COUR;

char tab[taille_tab][20]={"if","else","while","repeat","for","in" ,"break","print","function","c","FALSE","TRUE"};

char tab_t[taille_tab][20]={"if_token","else_token","while_token","repeat_token","for_token", "in_token", "break","print_token","function_token","c_token","bool_token","bool_token"};


char tab_sym[taille_tab_sym][5]={";",".",":","+","-","*","/",",","=","<-","==","<","<=",">",">=","!=","(",")","{","}","\""} ;


char tab_sym_t[taille_tab_sym][20] = {"PV_TOKEN","PT_TOKEN","BIPT_TOKEN","PLUS_TOKEN","MOINS_TOKEN","MULT_TOKEN","DIV_TOKEN","VIR_TOKEN","AFF_TOKEN","AFF_TOKEN"
,"EG_TOKEN","INF_TOKEN","INFEG_TOKEN","SUP_TOKEN","SUPEG_TOKEN","DIF_TOKEN","PO_TOKEN","PF_TOKEN","AO_TOKEN","AF_TOKEN","doubleQ_TOKEN"};

//function
typedef  struct {       char NOM[20];   int nbparam ; char param[30][20]; } TAB_IDF;
TAB_IDF TAB_IDFS2[NUMBER_VARS];

int comptTabFunct=0;

//Oussama
char TableTest[100][100], TableErreur[100][100];
int TableTestCpt=0,TableErreurCpt=0;
//oussama
int symCmpt = 0;
int affecIndex = 0;
int s;
int ero = 0;
char car_cour;
//int passer_comment();
void lire_car();
void Lire_mots();
void Lire_nombre();
void Lire_string(char first);
void Lire_sym();

//Semantique functions
int testVar(char* nom);
void addSymID();
void addSymType(TSYM type);
void test_conflict(TSYM type);
void generateConflictTypeError();
int testIdFunction(char * nom);
void addTabFunction(char* nom);
void lireCommentaire();

//Semantique variables
char tempVarID[100];
int tempTypeID = -1;
int finalTypeID = -1;
int conflictTypeOperator = 0;
int indexRedeclaredFunction = 0;

void Test_Symbole (char* cl);
void PROGRAM();
void INST();
void INSTS();
void IF();
void ELSE();
void function_def();
void assignement();
void function_call();
void parameter_list_call();
void parameter_list_def();
void COND();
void EXPR();
void WHILE();
void FOR();
void REPEAT();
void FOR_COND();
void AFFEC();
void TERM();
void FACT();
void PRINT();
void vect();
void id_term();
int pile[20];
int pos = -1;

void Sym_Suiv();
FILE* file;

//function
int testIdFunction(char * nom) {
    int exist = -1;
    for(int k = 0; k < NUMBER_VARS; k++)
    {
        if(strcmp(nom,TAB_IDFS2[k].NOM)==0)
        {
            exist = k;
            k=NUMBER_VARS;
        }
    }
    return exist;
}
void addTabFunction2(char* nom)
{
    strcpy(TAB_IDFS2[comptTabFunct].NOM, nom);
    comptTabFunct++;
}
//function index in table if we redeclare function
void addTabFunction(char* nom)
{

    int index = testIdFunction(nom);
    //if ID doesn't exist in Funct_table
    if(index == -1){
        strcpy(TAB_IDFS2[comptTabFunct].NOM, nom);
        indexRedeclaredFunction = comptTabFunct++;
    }else{
        //else erise data
    TAB_IDFS2[index].nbparam = 0;
    indexRedeclaredFunction = index ;
    }

}
//semantique functions definition
void addSymID() {
    int index = testVar(SYM_COUR.NOM);
    if(index == -1) {
        strcpy(TAB_IDFS[symCmpt].NOM, SYM_COUR.NOM);
        affecIndex = symCmpt++;
    }
    else {
        affecIndex = index;
    }
}

void addSymType(TSYM type) {
    TAB_IDFS[affecIndex].TIDF = type;
    affecIndex = 0;
}


//semantique funtion definition end
int Line = 1;
void Sym_Suiv(){

    while (car_cour == 32 ||  car_cour == '\t'){
        lire_car();
    }
    if(car_cour == '#') {
        strcpy(TableTest[TableTestCpt++],"comment detected");
        lireCommentaire();
        strcpy(TableTest[TableTestCpt++],"comment passed");
    }

    if(car_cour == '\n')
    {
        Line++;
        SYM_COUR.CODE="RETOUR_TOKEN";
        lire_car();

        return;
    }

    if(car_cour == EOF)
    {
        SYM_COUR.CODE="FIN_TOKEN";
        return;
    }
    if(car_cour == '"') {
        Lire_string('"');
    }
    else if (car_cour == 39) {
        Lire_string(39);
    }
    else if (isalpha(car_cour) != 0) {
        //printf("gonna read a alpha  \n");
        Lire_mots();
    }
    else if (isdigit(car_cour) != 0) {

        //printf("gonna read a number  \n");
        Lire_nombre();
    }
    else if (car_cour==EOF) {
        return;
    }
    else {
        //printf("gonna read a sym  \n");
        Lire_sym();
    }
}




void lire_car(){
    car_cour = fgetc(file);
}

void Lire_string(char first) {
    int cpt = 0;
    char c[100];
    int i = 0;
    int err = 0;
    lire_car();
    while(car_cour != first) {
        if(car_cour == 92) {
            c[i] = car_cour;
            lire_car();
            if(car_cour == first) {
                lire_car();
                c[i] = car_cour;
                i++;
            } else {
                i++;
                c[i] = car_cour;
            }
        }
        else if (car_cour == EOF) {
            err = 1;
            break;
        } else {
            c[i] = car_cour;
            lire_car();
            i++;
        }
    }
    if(err == 0) {
        c[i] = '\0';
        SYM_COUR.CODE = "STRING_TOKEN";
        strcpy(SYM_COUR.NOM, c);
    } else {
        ero++;
        SYM_COUR.CODE = "STRING_TOKEN_ERR";
        strcpy(SYM_COUR.NOM, "STRING_TOKEN_ERR");
    }
    lire_car();
}

void Lire_mots(){
    char c[100];
    int j = 0;
    int i = 0;

    c[i] = car_cour;

    while (car_cour != 32 && car_cour != '\n' && car_cour != '//' && car_cour != '/*' && (isalnum(car_cour) != 0 || car_cour == '_' || car_cour == '.') ) {
        c[i]=car_cour;
        lire_car();
        i++;
    }
    c[i]='\0';

    for(i=0;i<taille_tab;i++)
    {
        if (strcmp(c,tab[i])==0)
        {
            j=1;
            SYM_COUR.CODE=tab_t[i];
            strcpy(SYM_COUR.NOM, c);
        }

        if((i==(taille_tab-1)) && (j==0))
        {
        SYM_COUR.CODE="ID_TOKEN";
        strcpy(SYM_COUR.NOM,c);

        }
    }
    if(strcmp(SYM_COUR.CODE,"c_token")==0)
        {
            if(car_cour!='(')
            {
                SYM_COUR.CODE="ID_TOKEN";
                strcpy(SYM_COUR.NOM,c);
            }
        }
}

void Lire_nombre(){
    int cpt=0;
    char h[100];
    while (isdigit(car_cour)!=0)
       {
        h[cpt]=car_cour;
         lire_car();

         cpt++;

       }

    if(car_cour==32 || car_cour=='\n'|| isalnum(car_cour)==0)
            {
            strcpy(SYM_COUR.NOM,h);
            SYM_COUR.CODE="NUM_TOKEN";
            SYM_COUR.val=atoi(SYM_COUR.NOM);
           /* printf("%d\n",SYM_COUR.val); */
            }

    else if (isalpha(car_cour)!=0)
             {
                 while(isalpha(car_cour)!=0)
                {
                lire_car();
                }
                 ero++;
             }

}

void Lire_sym(){

    char c[2]={0};
    int j = 0;
    int i=0;
    int d = 0;
    int k =0;
    c[0]=car_cour;
    int z=0;
    lire_car();
    if(car_cour!=32 && car_cour!='\n' && isalnum(car_cour)==0  )
    {
        c[1]=car_cour;
        c[2]='\0';

        for( i=0;i<taille_tab_sym;i++)
        {   if (strcmp(c,tab_sym[i])==0){
                j=1;
                k=1;
                //printf("sym trouve %s \n",c);
                SYM_COUR.CODE=tab_sym_t[i];
                lire_car();
        }
        }

        if ((i==taille_tab_sym )&& (k ==0))
            {
                c[1]='\0';
                for(j=0;j<taille_tab_sym;j++)

                {if (strcmp(c,tab_sym[j])==0){
                    z=1;
                    //printf("sym trouve %s \n",c);
                    SYM_COUR.CODE=tab_sym_t[j];
                }}

                if(z==0 && j==taille_tab_sym)
                {
                    ero++;

                }
        }

    }
    else {
        if (car_cour==32 || car_cour=='\n' || isalnum(car_cour)!=0){
        for(i=0;i<taille_tab_sym;i++){
            c[1]='\0';
            if (strcmp(c,tab_sym[i])==0){
                d=1;
                //printf("sym trouve %s \n",c);
                SYM_COUR.CODE=tab_sym_t[i];
            }
            if (d==0 && i==taille_tab_sym){

                ero++;

            }

        }


    }
}
}



void Test_Symbole(char* cl){
    //printf("On compare %s avec  %s  \n",SYM_COUR.CODE,cl);
    sprintf(TableTest[TableTestCpt++], "#line %d : This token was read : %s  ", Line, SYM_COUR.CODE);
if (strcmp(SYM_COUR.CODE,cl)==0)
{
    //printf("%s  \t c_est correc \n",SYM_COUR.CODE);
    Sym_Suiv();
    //printf("Le symbole suivant est : %s\n", SYM_COUR.CODE);
}
else
    {
        printf("%s",SYM_COUR.CODE);
        //printf("ERREUR1");
        sprintf(TableErreur[TableErreurCpt++], "#line %d : UNEXPECTED %s TOKEN in Instruction",Line, SYM_COUR.CODE);
        ero++;
    }
}

void PROGRAM()
{
    //printf("Programme debut \n");
    strcpy(TableTest[TableTestCpt++],"Begin Parsing");
    lire_car();
    Sym_Suiv();
    INSTS();
    if(strcmp(SYM_COUR.CODE, "FIN_TOKEN") == 0)
      //  printf("finished parsing\n");
      strcpy(TableTest[TableTestCpt++],"finished parsing");
}


void INST()
{
    //printf("INST debut \n");
    strcpy(TableTest[TableTestCpt++],"Debut Instruction");
    if(strcmp(SYM_COUR.CODE,"ID_TOKEN") == 0)
    {
        id_term();
    }
    else if(strcmp(SYM_COUR.CODE,"if_token") == 0) {
        IF();
    }
    else if(strcmp(SYM_COUR.CODE,"while_token") == 0) {
        WHILE();
    }
    else if(strcmp(SYM_COUR.CODE,"print_token") == 0) {
        PRINT();
    }
    else if(strcmp(SYM_COUR.CODE, "STRING_TOKEN") == 0) {
        Test_Symbole("STRING_TOKEN");
    }
    else if(strcmp(SYM_COUR.CODE, "NUM_TOKEN") == 0) {
        Test_Symbole("NUM_TOKEN");
    }
    else if(strcmp(SYM_COUR.CODE, "bool_token") == 0) {
        Test_Symbole("bool_token");
    }
    else if(strcmp(SYM_COUR.CODE, "c_token") == 0) {
        vect();
    }
    else if(strcmp(SYM_COUR.CODE,"repeat_token") == 0)
    {
        REPEAT();
    }
    else if(strcmp(SYM_COUR.CODE,"for_token") == 0)
    {
        FOR();
    }
    else if(strcmp(SYM_COUR.CODE,"break") == 0)
    {
        Test_Symbole("break");
    }
    else if(strcmp(SYM_COUR.CODE, "RETOUR_TOKEN") == 0 || strcmp(SYM_COUR.CODE, "FIN_TOKEN") == 0) {
    }
    else if(strcmp(SYM_COUR.CODE, "AF_TOKEN") == 0) {

    }
    else {
        //printf("UNEXPECTED %s TOKEN in INST\n", SYM_COUR.CODE);
        sprintf(TableErreur[TableErreurCpt++], "#line %d : UNEXPECTED %s TOKEN in Instruction",Line, SYM_COUR.CODE);
        Sym_Suiv();
    }
    //printf("finished inst\n");
    strcpy(TableTest[TableTestCpt++],"finished instruction");
}

void function_call() {
    Test_Symbole("PO_TOKEN");
    parameter_list_call();
    Test_Symbole("PF_TOKEN");
}

//temporary function
void generateConflictTypeError() {
    if(conflictTypeOperator == 1) {
        //printf("ERRREUUUUUUUUUR conflictTypeOperator\n");
        strcpy(TableErreur[TableErreurCpt++],"Error : conflictTypeOperator");
        conflictTypeOperator = 0;
    } else {
        finalTypeID = tempTypeID;
    }
    tempTypeID = -1;
}

void vect() {
    Test_Symbole("c_token");
    Test_Symbole("PO_TOKEN");
    if(strcmp(SYM_COUR.CODE, "NUM_TOKEN") == 0) {
        EXPR();
        generateConflictTypeError();
        while(strcmp(SYM_COUR.CODE, "VIR_TOKEN") == 0) {
            Sym_Suiv();
            EXPR();
            generateConflictTypeError();
        }
    }
    else if(strcmp(SYM_COUR.CODE, "STRING_TOKEN") == 0) {
        Sym_Suiv();
        while(strcmp(SYM_COUR.CODE, "VIR_TOKEN") == 0) {
            Sym_Suiv();
            Test_Symbole("STRING_TOKEN");
        }
    }
    else if(strcmp(SYM_COUR.CODE, "bool_token") == 0) {
        EXPR();
        generateConflictTypeError();
        while(strcmp(SYM_COUR.CODE, "VIR_TOKEN") == 0) {
            Sym_Suiv();
            EXPR();
            generateConflictTypeError();
        }
    }
    else if(strcmp(SYM_COUR.CODE, "ID_TOKEN") == 0) {
        EXPR();
        generateConflictTypeError();
        while(strcmp(SYM_COUR.CODE, "VIR_TOKEN") == 0) {
            Sym_Suiv();
            EXPR();
            generateConflictTypeError();
        }
    }
    else if(strcmp(SYM_COUR.CODE, "c_token") == 0) {
        vect();
    }

    Test_Symbole("PF_TOKEN");
}

void id_term() {
    int index = 0;
    strcpy(tempVarID, SYM_COUR.NOM);
    Test_Symbole("ID_TOKEN");
    if(strcmp(SYM_COUR.CODE, "AFF_TOKEN") == 0) {
        assignement();
        if(strcmp(SYM_COUR.CODE, "RETOUR_TOKEN") && strcmp(SYM_COUR.CODE, "FIN_TOKEN")) {
            //printf("UNEXPECTED %s\n", SYM_COUR.CODE);
            sprintf(TableErreur[TableErreurCpt++], "#line %d : UNEXPECTED %s",Line, SYM_COUR.CODE );
        }
        //printf("final type after assignement %s\n", toshowType[finalTypeID]);
        sprintf(TableTest[TableTestCpt++], "#line %d : final type after assignement %s",Line, toshowType[finalTypeID]);
        //printf("finished assignement\n");
    }
    else if(strcmp(SYM_COUR.CODE, "PO_TOKEN") == 0) {
            int funct_exists = testIdFunction(tempVarID);
            if(funct_exists == -1){
                //printf("Error: Function not declared\n");
                sprintf(TableErreur[TableErreurCpt++],"Line %d : Function not declared",Line);
            }
        function_call();
    }
    else if(!strcmp(SYM_COUR.CODE, "RETOUR_TOKEN") || !strcmp(SYM_COUR.CODE, "FIN_TOKEN")) {

        index = testVar(tempVarID);
        if(index == -1) {
          //  printf("TOKEN %s NOT DEFINED\n", tempVarID);
            sprintf(TableErreur[TableErreurCpt++], "#line %d : TOKEN %s NOT DEFINED",Line -1, tempVarID);
        }

    }
    // else if(!strcmp(SYM_COUR.CODE, "PLUS_TOKEN") || !strcmp(SYM_COUR.CODE, "MOINS_TOKEN") ||
    //         !strcmp(SYM_COUR.CODE, "MULT_TOKEN") || !strcmp(SYM_COUR.CODE, "DIV_TOKEN")) {
    //     Sym_Suiv();
    //     EXPR();
    // }
}

int testVar(char* nom) {
    for(int i = 0; i < NUMBER_VARS; i++) {
        if(strcmp(nom, TAB_IDFS[i].NOM) == 0) {
            return i;
        }
    }
    return -1;
}

void assignement()
{
    addSymID(tempVarID);
    Test_Symbole("AFF_TOKEN");
    if(strcmp(SYM_COUR.CODE, "function_token") == 0)
    {
        addTabFunction(tempVarID);
        function_def();
    }
    else {
        EXPR();
        generateConflictTypeError();
        addSymType(finalTypeID);
    }
}

void INSTS()
{
    //printf("INSTS debut \n");
    INST();
    while(strcmp(SYM_COUR.CODE,"RETOUR_TOKEN")==0)
    {
       //   printf("a l'inst suivant' \n");
        Test_Symbole("RETOUR_TOKEN");
        INST();
    }
}


void IF()
{
    Test_Symbole("if_token");
    Test_Symbole("PO_TOKEN");
    COND();
    Test_Symbole("PF_TOKEN");
    while(strcmp(SYM_COUR.CODE,"RETOUR_TOKEN")==0)
    {
        Sym_Suiv();
    }
    if(strcmp(SYM_COUR.CODE,"AO_TOKEN")==0)
    {
        Test_Symbole("AO_TOKEN");
        INSTS();
        Test_Symbole("AF_TOKEN");
        while(strcmp(SYM_COUR.CODE,"RETOUR_TOKEN")==0)
    {
        Sym_Suiv();

    }
        if(strcmp(SYM_COUR.CODE,"else_token")==0)
        {
            ELSE();
        }
        else {
            if(strcmp(SYM_COUR.CODE, "FIN_TOKEN")) {
                INST();
            }
        }
    }
    else
    {
        INST();
    }
}


void ELSE()
{
    Test_Symbole("else_token");
    if(strcmp(SYM_COUR.CODE,"if_token")==0){
            IF();
    }else if(strcmp(SYM_COUR.CODE,"AO_TOKEN")==0){
        Test_Symbole("AO_TOKEN");
        INSTS();
        Test_Symbole("AF_TOKEN");

    }

}

void function_def()
{
    Test_Symbole("function_token");
    Test_Symbole("PO_TOKEN");
    parameter_list_def();
    Test_Symbole("PF_TOKEN");
    Test_Symbole("AO_TOKEN");
    Sym_Suiv();
    //printf("%s\n", SYM_COUR.CODE);
    INSTS();
    //printf("FINISHED FUNCTION INSTS\n");
    Test_Symbole("AF_TOKEN");
}

void parameter_list_def() {
    while(!strcmp(SYM_COUR.CODE, "ID_TOKEN")) {
        int cpt = TAB_IDFS2[indexRedeclaredFunction].nbparam++;
        strcpy(TAB_IDFS2[indexRedeclaredFunction].param[cpt],SYM_COUR.NOM);
        Test_Symbole("ID_TOKEN");
        if(!strcmp(SYM_COUR.CODE, "VIR_TOKEN")) {
            Test_Symbole("VIR_TOKEN");
            if(!strcmp(SYM_COUR.CODE, "PF_TOKEN")) {
                //printf("UNEXPECTED %s TOKEN\n", SYM_COUR.CODE);
                sprintf(TableErreur[TableErreurCpt++], "#line %d : UNEXPECTED %s",Line, SYM_COUR.CODE );
                break;
            }
        }
        else {
            break;
        }
    }
}

void parameter_list_call()
{       int cptParam = 0;
    while(!strcmp(SYM_COUR.CODE, "ID_TOKEN") || !strcmp(SYM_COUR.CODE, "NUM_TOKEN") || !strcmp(SYM_COUR.CODE, "c_token")
        || !strcmp(SYM_COUR.CODE, "STRING_TOKEN") || !strcmp(SYM_COUR.CODE, "bool_token")) {
        //if parameter is vector
        if(!strcmp(SYM_COUR.CODE, "c_token")) {
            vect();

        }
        else if(!strcmp(SYM_COUR.CODE, "ID_TOKEN")){
                if(testVar(SYM_COUR.NOM)==-1){
                //printf("var non declare \n");
                sprintf(TableErreur[TableErreurCpt++],"#Line %d : Variable %s is not declared",Line,SYM_COUR.NOM);
                   }

                Test_Symbole("ID_TOKEN");
            if(!strcmp(SYM_COUR.CODE, "PO_TOKEN")) {
            function_call();
            Sym_Suiv();}
        }else
        {
                Sym_Suiv();
        }
        cptParam++;
        if(!strcmp(SYM_COUR.CODE, "VIR_TOKEN")) {
            Sym_Suiv();
            if(!strcmp(SYM_COUR.CODE, "PF_TOKEN")) {
               // printf("UNEXPECTED %s TOKEN\n", SYM_COUR.CODE);
               sprintf(TableErreur[TableErreurCpt++], "#line %d : UNEXPECTED %s",Line, SYM_COUR.CODE );
                break;
            }
        } else {
            break;
        }
    }
    if(cptParam != TAB_IDFS2[comptTabFunct-1].nbparam){
        //printf("Error : Parameter numbers is incorrect", SYM_COUR.CODE);
        sprintf(TableErreur[TableErreurCpt++],"Line %d : FunctionParameters numbers is incorrect",Line);
    }
}

void EXPR()
{
    TERM();
    while ((strcmp(SYM_COUR.CODE,"PLUS_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"MOINS_TOKEN")==0))
           {
               char* pm[20];
               strcpy(pm,SYM_COUR.CODE);
               Sym_Suiv();
               TERM();
            }
    //printf("final type after expression %s\n", toshowType[finalTypeID]);
    sprintf(TableTest[TableTestCpt++], "#line %d : final type after assignement %s",Line, toshowType[finalTypeID]);
}
void TERM()
{
    FACT();
    while ((strcmp(SYM_COUR.CODE,"MULT_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"DIV_TOKEN")==0))
           {
                char* md[20];
               strcpy(md,SYM_COUR.CODE);
                Sym_Suiv();
                FACT();
            }
}

void test_conflict(TSYM type) {
    switch(type) {
        case NUM:
            if(tempTypeID == -1) {
                tempTypeID = NUM;
            } else if(tempTypeID != NUM && tempTypeID != VECT && tempTypeID != BBOOL) {
                    conflictTypeOperator = 1;
            } else if(tempTypeID == VECT) {
                tempTypeID = VECT;
            } else if(tempTypeID == NUM) {
                tempTypeID = NUM;
            }
            break;
        case STR:
            if(tempTypeID == -1) {
                tempTypeID = STR;
            } else {
                conflictTypeOperator = 1;
            }
            break;
        case BBOOL:
            if(tempTypeID == -1) {
                tempTypeID = BBOOL;
            } else if(tempTypeID != BBOOL && tempTypeID != NUM && tempTypeID != VECT) {
                conflictTypeOperator = 1;
            } else if(tempTypeID == VECT) {
                tempTypeID = VECT;
            } else if(tempTypeID == NUM || tempTypeID == BBOOL) {
                tempTypeID = NUM;
            }
            break;
        case VECT:
            if(tempTypeID == -1) {
                tempTypeID = VECT;
            } else if(tempTypeID != NUM && tempTypeID != VECT && tempTypeID != BBOOL) {
                conflictTypeOperator = 1;
            } else {
                tempTypeID = VECT;
            }
            break;
        default:
            //printf("called with unrecognized type\n");
            strcpy(TableErreur[TableErreurCpt++],"called with unrecognized type");
    }
}

void FACT()
{
    int index = 0;
    if(strcmp(SYM_COUR.CODE,"ID_TOKEN")==0) {
        //test if variable is already defined
        strcpy(tempVarID, SYM_COUR.NOM);
        Test_Symbole("ID_TOKEN");
        //code function call
        if(strcmp(SYM_COUR.CODE, "PO_TOKEN") == 0) {
             int funct_exists = testIdFunction(tempVarID);
            if(funct_exists == -1){
               // printf("Error: Function not declared\n");
                sprintf(TableErreur[TableErreurCpt++],"Line %d : Function not declared",Line);
            }
            function_call();
        } else {
            index = testVar(tempVarID);
            if(index == -1) {
               // printf("-- -- TOKEN %s NOT DEFINED\n", SYM_COUR.NOM);
               sprintf(TableErreur[TableErreurCpt++], "#line %d : TOKEN %s IS NOT DEFINED",Line, SYM_COUR.NOM );
            }
            int idType = TAB_IDFS[index].TIDF;
            test_conflict(idType);
        }
    }
    else if(strcmp(SYM_COUR.CODE,"NUM_TOKEN")==0)
    {
        Test_Symbole("NUM_TOKEN");

        //semantic type conflict testing
        test_conflict(NUM);
    }
    else if(strcmp(SYM_COUR.CODE,"STRING_TOKEN")==0) {
        Test_Symbole("STRING_TOKEN");

        //semantic type conflict testing
        test_conflict(STR);
    }
    else if(strcmp(SYM_COUR.CODE,"bool_token")==0) {
        Test_Symbole("bool_token");

        //semantic type conflict testing
        test_conflict(BBOOL);

    }
    else if(strcmp(SYM_COUR.CODE,"c_token")==0) {
        vect();

        //semantic type conflict testing
        test_conflict(VECT);
    }
    else if(strcmp(SYM_COUR.CODE,"PO_TOKEN")==0)
    {
        Sym_Suiv();
        EXPR();
        generateConflictTypeError();
        Test_Symbole("PF_TOKEN");
    }
}



void COND()
{char* tok[20];

    EXPR();
    generateConflictTypeError();
    if((strcmp(SYM_COUR.CODE,"PF_TOKEN")==0))
    {
        return;
    }
    if((strcmp(SYM_COUR.CODE,"EG_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"INF_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"INFEG_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"SUP_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"SUPEG_TOKEN")==0)||(strcmp(SYM_COUR.CODE,"DIF_TOKEN")==0))
    {
        strcpy(tok,SYM_COUR.CODE);
        Test_Symbole(SYM_COUR.CODE);
    }
    else
    {
        //printf("%s\n", SYM_COUR.CODE);
        //printf("ERREUR5\n");
        sprintf(TableErreur[TableErreurCpt++], "#line %d : UNEXPECTED %s TOKEN in Condition",Line, SYM_COUR.CODE);
        ero++;
    }
    EXPR();
    generateConflictTypeError();

}

void WHILE()
{
    //printf("While debut \n");
    Test_Symbole("while_token");
    Test_Symbole("PO_TOKEN");
    COND();
    Test_Symbole("PF_TOKEN");
    while(strcmp(SYM_COUR.CODE,"RETOUR_TOKEN")==0)
    {
        Sym_Suiv();
    }
    if(strcmp(SYM_COUR.CODE,"AO_TOKEN")==0)
    {
        Test_Symbole("AO_TOKEN");
        INSTS();
        Test_Symbole("AF_TOKEN");
    }
    else
    {
        INST();
    }

}

void REPEAT(){
        Test_Symbole("repeat_token");
        Test_Symbole("AO_TOKEN");
        INSTS();
        Test_Symbole("AF_TOKEN");
 }

void FOR_COND(){
    //printf("inside for condition");
    Test_Symbole("ID_TOKEN");
    Test_Symbole("in_token");
    if(strcmp(SYM_COUR.CODE,"NUM_TOKEN")==0) Sym_Suiv();
    // prendre first NUM
    Test_Symbole("BIPT_TOKEN");
    if(strcmp(SYM_COUR.CODE,"NUM_TOKEN")==0) Sym_Suiv();
    //prendre second NUM

}

void FOR(){
    Test_Symbole("for_token");
    Test_Symbole("PO_TOKEN");
    FOR_COND();
    Test_Symbole("PF_TOKEN");
    while(strcmp(SYM_COUR.CODE,"RETOUR_TOKEN")==0)
    {
        Sym_Suiv();
    }
    if(strcmp(SYM_COUR.CODE,"AO_TOKEN")==0)
    {
        Test_Symbole("AO_TOKEN");
        INSTS();
        Test_Symbole("AF_TOKEN");
    }
    else
    {
        INST();
    }
}


void PRINT()
{
    Test_Symbole("print_token");
    Test_Symbole("PO_TOKEN");
    EXPR();
    generateConflictTypeError();
    Test_Symbole("PF_TOKEN");
}

void PrintTabTest(){
        printf("********* Program Tests **********\n");
    for(int i =0 ; i<TableTestCpt ; i++){
        printf(TableTest[i]);
        printf("\n");
    }

}
void PrintTabError(){
        printf("*********  Program Errors ************\n");
        for(int i =0 ; i<TableErreurCpt ; i++){
        printf(TableErreur[i]);
        printf("\n");
    }

}

void lireCommentaire(){

    lire_car();
    while(car_cour!='\n')
    {
        lire_car();
    }
}

int main(){
    file = fopen("Rcode.txt","r");
    PROGRAM();

    PrintTabTest();
    PrintTabError();
}
