000100960221     H DECEDIT('0,') DATEDIT(*DMY.)
000200960221?     *--------------------------------------------------------------*
000300021010      * BAK850U3                                                     *
000400960221      *  *---------------------------------------------------------* *
000500021010      *    profilo abilitato a tutti i records di KPLOG           *
000501021010      *    ** abilita le funzioni di modifica)                    *
000600021010      *    - l'abilitazione viene data a utenti del gruppo EDPSE     *
000601021010      *    - oppure se il flag §UTEMAZ di abilitazione su AZUTE00f   *
000700021010      *      è = a S                                               * *
000900960221?     *  DS                                                          *
001000960221     D KPJBA         E DS
001002021010     D tibs34ds      E DS                  inz
001003021010     D ddatiute      e ds
001004021010     D azuteds       e ds                  extname(AZUTE00F)
001102021010     D DUTE01        E DS
002100960221?     *--------------------------------------------------------------*
002200960221?     *  CICLO PRINCIPALE                                            *
002300960221?     *--------------------------------------------------------------*
003602021010     C                   MOVEL     utefaf        dute01
003603021010     c     utefun        ifeq      'EDPSE'
003604021010     c     §UTEMAZ       oreq      'S'
003605021010     c                   movel     *blank        OKKO
003606021010     c                   endif
005100020923     C*
005300960221      *
005400960221     C                   SETON                                        LR
005500960221?     *--------------------------------------------------------------*
005600960221?     *  *INZSR: Operazioni iniziali                                 *
005700960221?     *--------------------------------------------------------------*
005800960221     C     *INZSR        BEGSR
005900960221     C     *ENTRY        PLIST
006000960221     C                   PARM                    KPJBA
006001021010     C                   PARM                    OKKO                           KO = no
006002021010     c                   movel     'KO'          OKKO              2
006003021010     c     *dtaara       define    §azute        azuteds
006004021010     c     *dtaara       define    §datiute      ddatiute
006005021010      *
006006021010     c                   IN(E)     *DTAARA
006007021010if  1c                   IF        %Error
006008021010     c                   EVAL      I34Tla = 'L'
006009021010     c                   CALL      'TIBS34R'
006010021010     c                   PARM                    Tibs34Ds
006011021010     c                   IN        *DTAARA
006012021010     c                   ENDIF
006100960221     C                   ENDSR
