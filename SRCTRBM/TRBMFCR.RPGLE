000100080711     H dftactgrp(*no) actgrp(*caller)
000200990908
000300080711     FTRBMFCIF  IF   E             DISK
000400080711     FTRBMFCOF  O  A E             DISK
000500990908
000600030403     D*------------
000700030403     D* VARIABILI DI WRK
000800030403     D*------------
000900030404     D rigaPrec        S            256    inz(*blanks)
001000030404     D rigaCorr        S            256    inz(*blanks)
001100030404     D rigaWrk         S              1    dim(256) inz(*blanks)
001200030403
001300030403     C*------------
001400030403     C* MAIN
001500030403     C*------------
001600030403     C*
001700030403     C* ESEGUO "NORMALIZZAZIONE"
001800030403     C                   EXSR      NORMFIL
001900030403     C
002000000613     C*
002100030403     C                   SETON                                        LR
002200000613
002300000724
002400910830     C*--------------------------------------------------------
002500030403     C* NORMFILE - Esegue "normalizzazione"                   *
002600910830     C*--------------------------------------------------------
002700030403     C     NORMFIL       BEGSR
002800990910     C*
002900030403     C* Eseguo la prima lettura fuori ciclo x stabilire la 1° riga "precdente"
003000080711     C                   read      TRBMFCIF
003100030403     C                   eval      rigaPrec = FCIDTA
003200030403     C*
003300030403     C* Ciclo su tutto il file
003400080711     C                   dow       not %eof(TRBMFCIF)
003500030403     C* Eseguo le successive letture x stabilire le righe "correnti"
003600080711     C                   read      TRBMFCIF
003700030403     C                   eval      rigaCorr = FCIDTA
003800030403     C* Verifico se presenti caratteri speciali di formattazione *FCFC grassetto (carattere '+') nel
003900030403     C* sulla riga corrente
004000030403     C                   if        %subst(rigaCorr:1:1) = '+'
004100030403     C* Quindi eseguo "normalizzazione" delle righe
004200030404     C     1             do        256           i                 3 0
004300030403     C                   if        %subst(rigaPrec:i:1) <> *blanks AND
004400030403     C                             %subst(rigaCorr:i:1) =  *blanks
004500030403     C                   eval      rigaWrk(i) = %subst(rigaPrec:i:1)
004600030403     C                   else
004700030403     C                   if        %subst(rigaPrec:i:1) =  *blanks AND
004800030403     C                             %subst(rigaCorr:i:1) <> *blanks
004900030403     C                   eval      rigaWrk(i) = %subst(rigaCorr:i:1)
005000030403     C                   else
005100030515     C                   if        %subst(rigaPrec:i:1) <> *blanks AND
005200030515     C                             %subst(rigaCorr:i:1) <> *blanks
005300030515     C                   eval      rigaWrk(i) = %subst(rigaPrec:i:1)
005400030515     C                   else
005500030515     C                   eval      rigaWrk(i) = %subst(rigaCorr:i:1)
005600030515     C                   endif
005700030403     C                   endif
005800030403     C                   endif
005900030403     C                   enddo
006000030403     C* Dopo la "normalizzazione" la riga di wrk diventa la riga precedente
006100030403     C                   movea     rigaWrk       rigaPrec
006200030403     C*
006300030526     C                   endif
006400030526     C*
006500030526     C* Se primo carattere riga (formattazione *FCFC) nn è '*' oppure siamo a file file di input
006600030526     C                   if        %subst(rigaCorr:1:1) <> '+' OR
006700080711     C                             %eof(TRBMFCIF)
006800030403     C* A questo punto "abblenco" comunque il primo byte della riga precedente se contiene il caratt
006900030403     C* di formattazione *FCFC grassetto (carattere '+')
007000030403     C                   if        %subst(rigaPrec:1:1) = '+'
007100030403     C                   eval      %subst(rigaPrec:1:1) = *blanks
007200030403     C                   endif
007300030403     C* Se nn presenti caratteri speciali di formattazione *FCFC grassetto (carattere '+') nel 1° by
007400030403     C* nella riga corrente inserisco la riga precedente
007500030403     C                   eval      FCODTA = rigaPrec
007600080711     C                   write     TRBMFCO0
007700030403     C* E la riga precedente diventa la riga corrente
007800030403     C                   eval      rigaPrec = rigaCorr
007900030403     C                   endif
008000030403     C* Quindi continuo la lettura del wrk file di input
008100030403     C                   enddo
008200000724     C*
008300910830     C                   ENDSR
