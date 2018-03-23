000100960221     H DECEDIT('0,') DATEDIT(*DMY.)
000200960221?     *--------------------------------------------------------------*
000300021010      * BAK850U3BA                                                   *
000400960221      *  *---------------------------------------------------------* *
000500021010      *    profilo abilitato a tutti i records di KPLOG           *
000501021010      *    ** abilita le funzioni di modifica)                    *
000600021010      *    - l'abilitazione viene data a utenti del gruppo EDPSE     *
000601021010      *    - oppure se il flag §UTEMAZ di abilitazione su AZUTE00f   *
000700021010      *      è = a S                                               * *
000900960221?     *  DS                                                          *
001000960221     D KPJBA         E DS
001100020923     D TIBS34        E DS                  EXTNAME(TIBS34DS)
001101021010     D azute         E DS                  EXTNAME(azute00f)
001102021010     D DUTE01        E DS
002100960221?     *--------------------------------------------------------------*
002200960221?     *  CICLO PRINCIPALE                                            *
002300960221?     *--------------------------------------------------------------*
003100020923     C                   CLEAR                   TIBS34
003200020924     C                   MOVEL     KNMUS         I34ute
003300020923     C                   MOVEL     'L'           I34TLA
003400020923     C                   MOVEL     'S'           I34DTA
003500020923     C                   CALL      'TIBS34R'                            31
003600020923     C                   PARM                    TIBS34
003601021010     C                   MOVEL     O34RUT        azute
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
006100960221     C                   ENDSR
