000100990901     H*------------------------------------------------------------------------*
000200000327     H* AGGIORNA I CAMPI "OLTRE 2" X QUESTIONE "POSTE"                         *
000300000308     H***********************************************************              *
000400000308     H***     ATTENZIONE!!! COMPILARE CON L'OPZIONE "PC"      ***              *
000500000308     H***********************************************************              *
000600000327     H***********************************************************              *
000700000327     H***********************************************************              *
000800000327     H***   ATTENZIONE!!! Impostare a programma (spec 95/97)  ***              *
000900000327     H***                 la versione di cui si desidera      ***              *
001000000327     H***                 effettuare l'aggiornamento.         ***              *
001100000327     H***********************************************************              *
001200990901     H*------------------------------------------------------------------------*
001300000327     H DATEDIT(*YMD)   EXPROPTS(*RESDECPOS)
001400990901     F*------------------------------------------------------------------------*
001500990901     F* DATA BASE
001600990901     F*------------------------------------------------------------------------*
001700000327     F*
001800990901     D*------------------------------------------------------------------------*
001900990901     D* SCHIERE
002000990901     D*------------------------------------------------------------------------*
002100990901     D*---
002200990901     D* ARCHITETTURA
002300990901     D*---
002400990901     D KPJBA         E DS
002500000203     D*---
002600000327     D* SCHIERA PER COSTRUZIONE STRINGHE DI COMANDO SQL
002700000203     D*---
002800000327     D  COMMAND        S             70    dim(4) CTDATA PERRCD(1)
002900000327     D*---
003000000327     D* SCHIERA PER REPERIMENTO ARCHIVI E NOMI CAMPO X COSTRUZIONE COMANDO SQL
003100000327     D*---
003200000327     D  FILES          S             20    dim(5) CTDATA PERRCD(1)
003300990901     C*------------------------------------------------------------------------*
003400990901     C* MAIN LINE
003500990901     C*------------------------------------------------------------------------*
003600990901     C*
003700000204     C* ESECUZIONE COMANDI NAVIGAZIONALI
003800000204     C                   EXSR      EXESQL
003900000204     C*
004000000204     C                   SETON                                        LR
004100000204     C*------------------------------------------------------------------------*
004200000204     C* EXESQL - ESECUZIONE COMANDI NAVIGAZIONALI
004300000204     C*------------------------------------------------------------------------*
004400000204     C     EXESQL        BEGSR
004500000204     C*
004600000327     C                   Z-ADD     1             i                 2 0
004700000327     C                   SETOFF                                       99
004800000327     C     FILES(i)      DOWNE     *blanks
004900000327     C                   MOVEL     *blanks       STRINGA         256
005000000327     C* Sostituzioni per composizione primo SQL
005100000327     C                   EVAL      %subst(COMMAND(1):18:5)=%trim(FILES(i))
005200000327     C                   EVAL      %subst(COMMAND(1):31:3)=%subst(FILES(i):3:3)
005300000327     C                   EVAL      %subst(COMMAND(1):38:3)=%subst(FILES(i):3:3)
005400000327     C                   EVAL      %subst(COMMAND(1):46:3)=%subst(FILES(i):3:3)
005500000327     C                   EVAL      %subst(COMMAND(1):53:3)=%subst(FILES(i):3:3)
005600000327     C                   EVAL      %subst(COMMAND(2):07:3)=%subst(FILES(i):3:3)
005700000327     C                   EVAL      %subst(COMMAND(2):20:3)=%subst(FILES(i):3:3)
005800000327     C*
005900000327     C                   EVAL      STRINGA=
006000000327     C                             (%trim(COMMAND(1))+' '+%trim(COMMAND(2)))
006100000327     C                   IF        *in99=*off
006200000327     C/EXEC SQL
006300000327     C+     EXECUTE IMMEDIATE :STRINGA
006400000327     C/END-EXEC
006500000327     C                   ENDIF
006600000327     C*
006700000327     C                   MOVEL     *blanks       STRINGA
006800000327     C* Sostituzioni per composizione secondo SQL
006900000327     C                   EVAL      %subst(COMMAND(3):18:5)=%trim(FILES(i))
007000000327     C                   EVAL      %subst(COMMAND(3):31:3)=%subst(FILES(i):3:3)
007100000327     C                   EVAL      %subst(COMMAND(3):38:3)=%subst(FILES(i):3:3)
007200000327     C                   EVAL      %subst(COMMAND(3):46:3)=%subst(FILES(i):3:3)
007300000327     C                   EVAL      %subst(COMMAND(3):53:3)=%subst(FILES(i):3:3)
007400000327     C                   EVAL      %subst(COMMAND(4):07:3)=%subst(FILES(i):3:3)
007500000327     C                   EVAL      %subst(COMMAND(4):20:3)=%subst(FILES(i):3:3)
007600000327     C                   EVAL      STRINGA=
007700000327     C                             (%trim(COMMAND(3))+' '+%trim(COMMAND(4)))
007800000327     C                   IF        *in99=*off
007900000327     C/EXEC SQL
008000000327     C+     EXECUTE IMMEDIATE :STRINGA
008100000327     C/END-EXEC
008200000327     C                   ENDIF
008300000327     C*
008400000327     C                   ADD       1             i
008500000203     C                   ENDDO
008600000203     C*
008700000204     C                   ENDSR
008800990901     C*------------------------------------------------------------------------*
008900990901     C* *INZSR - OPERAZIONI INIZIALI
009000990901     C*------------------------------------------------------------------------*
009100990901     C     *INZSR        BEGSR
009200990901     C*
009300990901     C* RICEVIMENTO PARAMETRI
009400990901     C     *ENTRY        PLIST
009500990901     C                   PARM                    KPJBA
009600000204     C*
009700990901     C                   ENDSR
009800000203     C*------------------------------------------------------------------------*
009900000327**CTDATA COMMAND (Occhio alle posizioni del testo x costruz. comando SQL, mantenere stessa formatt.)
010000000327UPDATE GAITRAAZP/XXCPX00F SET XXXLO2=XXXLOS, XXXZO2=XXXZOS
010100000327WHERE XXXLO2=0 AND XXXLOS<>0 AND XXXVER=00007
010200000327UPDATE GAITRAAZP/XXCPX00F SET XXXLO2=XXXLNA, XXXZO2=XXXZNC
010300000327WHERE XXXLO2=0 AND XXXLOS=0  AND XXXVER=00007
010400000327**CTDATA FILES (Occhio, lasciare sempre l'ultimo elem. di schiera a *blanks per uscire dal ciclo)
010500000327AZCPC
010600000327AZCPL
010700000327AZCPE
010800000327
