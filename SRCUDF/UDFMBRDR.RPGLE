000100990908     H dftactgrp(*yes)
000200031105
000300031105     ** CREATE FUNCTION GAITRAOBJ/UDFMBRDR
000400031105     ** ( FILE     CHAR(10)
000500031105     ** , MEMBRO   CHAR(10))
000600031105     ** RETURNS DECIMAL(10, 0)
000700031105     ** RETURNS NULL ON NULL INPUT
000800031105     ** LANGUAGE RPGLE
000900031105     ** NO SQL
001000031105     ** PARAMETER STYLE SQL
001100031105
001200031105
001300031105     D*------------------
001400031105     D* VARIABILI DI WRK
001500031105     D*------------------
001600031105     D InFile          S             10
001700031105     D InMembro        S             10
001800031105     D OutNumRec       S             10  0
001900031105     D NullInFile      S              5I 0
002000031105     D NullInMembro    S              5I 0
002100031105     D NullOutNumRec   S              5I 0
002200031105     D SQLState        S              5
002300031105     D FunctionName    S            139
002400031105     D SpecificName    S            128
002500031105     D MsgText         S             70
002600031105     D                                     VARYING
002700991104
002800991111     D mbrd01          s            266
002900991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
003000991111     D mbrd03          s              8    inz('MBRD0200')
003100991111     D mbrd04          ds
003200991111     D mbrd04a                       10                                         File
003300031105     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
003400991111     D mbrd05          s             10                                         Membro
003500991125     D mbrd06          s              1    inz('0')
003600991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
003700991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
003800000905
003900000905
004000000622      /TITLE *** MAIN ***
004100031105     C*
004200031105     C     *entry        plist
004300031105     ** Parametri di input.
004400031105     C                   parm                    InFile
004500031105     C                   parm                    InMembro
004600031105     ** Parametri di output.
004700031105     C                   parm                    OutNumRec
004800031105     ** Parametri di indicatore nullo.
004900031105     C                   parm                    NullInFile
005000031105     C                   parm                    NullInMembro
005100031105     C                   parm                    NullOutNumRec
005200031105     ** Parametri di funzione SQL.
005300031105     C                   parm                    SQLState
005400031105     C                   parm                    FunctionName
005500031105     C                   parm                    SpecificName
005600031105     C                   parm                    MsgText
005700031105     C*
005800031105     C                   clear                   NullOutNumRec
005900031105     C                   eval      SQLState = *ZERO
006000000622     C*
006100031105     C* Imposto i parametri x la chiamata all'API
006200031105     C                   eval      mbrd04a = InFile
006300031105     C                   eval      mbrd05  = InMembro
006400031105     C*
006500031105     C* Chiamata all'API
006600031105     C                   exsr      rtvmbrd
006700031105     C*
006800031105     C* Controllo se errore e se il libreria/file/membro ha dei record.
006900031105     C                   if        qusei = *blanks
007000031105     C                   eval      OutNumRec = qusnbrcr
007100031105     C                   else
007200031105     C                   eval      OutNumRec = *hival
007300031105     C***                eval      NullOutNumRec = -1
007400031105     C***                clear                   OutNumRec
007500031105     C***                eval      SQLState = '38I01'
007600031105     C***                eval      MsgText = 'Errore funzione UDFMBRDR'
007700031105     C                   endif
007800031105     C*
007900031105     C                   return
008000991026
008100000706
008200031105      /TITLE Reperisco informazioni libreria/file/membro.
008300991111     C     rtvmbrd       begsr
008400031105     C*
008500991111     C* Pulisco e imposto la DS degli errori.
008600000124     C                   clear                   qusec
008700991111     C                   eval      qusbprv = %size(qusec)
008800991111     C*
008900991111     C                   call      'QUSRMBRD'
009000991111     C     qusm0200      parm                    mbrd01
009100991111     C                   parm                    mbrd02
009200991111     C                   parm                    mbrd03
009300991111     C                   parm                    mbrd04
009400991111     C                   parm                    mbrd05
009500991111     C                   parm                    mbrd06
009600991111     C                   parm                    qusec
009700991111     C*
009800991111     C                   endsr
