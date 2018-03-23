000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060926      * FNLS03R1 *---------------------------------------------------*
000300060926      *  - PGM fittizio per scrivere la kpjbu dei lanci effettuati
000400060926      *    per monitorare eventuali errori
000500020418      *--------------------------------------------------------------*
000600060926     D KPJBA         E DS
000700000000     C     *ENTRY        PLIST
000800000000     C                   PARM                    KPJBA
000900060926     c
001000920916     C                   SETON                                        LR
