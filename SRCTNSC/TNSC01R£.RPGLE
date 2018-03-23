000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*----------------------------------------------------------------H
000300040331     FTNCSB03L  UF   E           K DISK
000400040331     FTNCSA01L  UF a E           K DISK
000500040331     Fazorg01L  iF   E           K DISK
000600040331     FTNSC01D   CF   E             WORKSTN
000700941028     F*----------------------------------------------------------------H
000800011113     D                SDS
000900011113     D NomPgm            *PROC
001000040331     D SK              S             78    DIM(5) CTDATA PERRCD(1)
001100040331     D COMODO          S              7  0 inz
001200941028     D*----------------------------------------------------------------
001300040331     D WLBDA8          DS
001400040331     D  G02DAT                 1      8  0
001500040331     D  G02INV                 9     16  0
001600040331     D  G02ERR                17     17
001700040331     D  G02TGI                18     22  0
001800040331     D TNSB50        E DS                  EXTNAME(TNSB50DS)
001900000000     D KPJBA         E DS
002000000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002100000000     C*---------------------------------------------------------------*
002200040331     C                   EXSR      SRCLEAR
002300020130     c*
002400020131     C* emette il primo video solo se non sono chiamato dal pgm TIBS20R
002500040331     C                   DO        *HIVAL
002600040331     C                   EXFMT     SC01D01
002700040331     C   KC              LEAVE
002800040331     C   KG              EXSR      VISCAS
002900040331     C   KH              EXSR      VISBOL
003000040331     C                   EXSR      SRCTR
003100040331     C   28              ITER
003200040331     c                   if        *inkf
003300040331     C                   EXSR      SRAGGIO
003400040331     C                   EXSR      SRclear
003500040331     c                   end
003600040331     C                   ENDDO
003700040331     C                   EVAL      *INLR = *ON
003800040331     C**********************************************************************
003900040331     C     SRCTR         BEGSR
004000040331     C**********************************************************************
004100040331     C                   SETOFF                                       28
004200040331     C                   MOVEA     '000000000'   *IN(01)
004300040331     c                   do
004400040331     C     vaas          IFEQ      0
004500040331     c                   seton                                        2801
004600040331     c                   movel     sk(3)         $msg
004700040331     c                   leave
004800040331     c                   end
004900941028     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
005000941028     C                   MOVEL     VAAS          WAA2              2 0
005100941028     C     WAA2          IFEQ      0
005200941028     C     VAAS          IFGT      60
005300941028     C                   ADD       1900          VAAS
005400941028     C                   ELSE
005500941028     C                   ADD       2000          VAAS
005600941028     C                   END
005700941028     C                   END
005800000000     C*
005900900420     C* LINEA DI PARTENZA DEVE ESISTERE SU AZORG
006000040331     C     vlnp          CHAIN     AZORG01L                           02
006100040331     C     *IN02         IFEQ      '1'
006200040331     C                   SETON                                        28
006300040331     C                   MOVEL     SK(1)         $MSG
006400040331     C                   leave
006500941028     C                   END
006600040331     C                   MOVEL     ORGDES        DESlnp
006700000000     C*
006800000000     C* NUMERO DI SPEDIZIONE
006900040331     C     vnsp          IFEQ      0
007000040331     C                   SETON                                        2803
007100040331     C                   MOVEL     SK(2)         $MSG
007200040331     C                   leave
007300941028     C                   END
007400000000     C*
007500040331     C* SE CONTRASSEGNO nono ESISTE SU TNCSB ERRORE
007600000000     C*
007700040331     C     KSPE          chain     TNCSB03l
007800040401     C                   IF        not %found(tncsb03l) or
007900040401     C                             (%found(tncsb03l) and csbddp <> 0)
008000040331     C                   SETON                                        280102
008100040331     C                   SETON                                        03
008200040331     C                   MOVEL     SK(5)         $MSG
008300040331     C                   leave
008400040331     C                   ELSE
008500040331     C     KSPE          chain     TNCSA01l
008600040331     C                   IF        %FOUND(TNCSA01L)
008700040331     C     vrsd          IFEQ      *BLANKS
008800040331     C                   eval      VRSD =        CSARSD
008900040331     C                   END
009000040331     C     vvid          ifEQ      *BLANKS
009100040331     C                   eval      VVID =        CSAVID
009200040331     C                   END
009300040331     C     vcad          ifEQ      *BLANKS
009400040331     C                   eval      VCAD =        CSACAD
009500040331     C                   END
009600040331     C     vcid          ifEQ      *BLANKS
009700040331     C                   eval      VCID =        CSACID
009800040331     C                   END
009900040331     C     vprm          ifEQ      *BLANKS
010000040331     C                   eval      VPRM =        CSAPRM
010100040331     C                   END
010200040331     C     vnzm          ifEQ      *BLANKS
010300040331     C                   eval      VNZM =        CSANZM
010400941031     C                   END
010500040331     C                   END
010600040331     C                   END
010700040331     C* riferimenti mittente
010800040331     C     vrsd          IFEQ      *BLANKS
010900040331     C                   SETON                                        2804
011000040331     C                   MOVEL     SK(04)        $MSG
011100040331     C                   leave
011200000000     C                   END
011300040331     C     vvid          ifEQ      *BLANKS
011400040331     C                   SETON                                        2805
011500040331     C                   MOVEL     SK(04)        $MSG
011600040331     C                   leave
011700040331     C                   END
011800040331     C     vcad          ifEQ      *BLANKS
011900040331     C                   SETON                                        2806
012000040331     C                   MOVEL     SK(04)        $MSG
012100040331     C                   leave
012200040331     C                   END
012300040331     C     vcid          ifEQ      *BLANKS
012400040331     C                   SETON                                        2807
012500040331     C                   MOVEL     SK(04)        $MSG
012600040331     C                   leave
012700040331     C                   END
012800040331     C     vprm          ifEQ      *BLANKS
012900040331     C                   SETON                                        2808
013000040331     C                   MOVEL     SK(04)        $MSG
013100040331     C                   leave
013200941031     C                   END
013300040331     c                   enddo
013400040331     c                   endsr
013500040331     c**********************************************************************
013600040331     c     sraggio       begsr
013700040331     c**********************************************************************
013800040331     C* CREO TNCSA00F
013900040331     C     KSPE          chain     TNCSA01l
014000040331     C                   Z-ADD     VAAS          CSAAAS
014100040331     C                   Z-ADD     VLNP          CSALNP
014200040331     C                   Z-ADD     VNSP          CSANSP
014300040331     C                   Z-ADD     VNRS          CSANRS
014400040331     C                   MOVEL     VRSD          CSARSD
014500040331     C                   MOVEL     VVID          CSAVID
014600040331     C                   MOVEL     VCAD          CSACAD
014700040331     C                   MOVEL     VCID          CSACID
014800040331     C                   MOVEL     VPRM          CSAPRM
014900040331     C                   MOVEL     VNZM          CSANZM
015000000000     C*
015100040331     c                   if        not %found(tncsa01l)
015200941028     C                   WRITE     TNCSA000
015300040331     c                   else
015400040331     C                   update    TNCSA000
015500040331     c                   end
015600040331     C* CODICE CLIENTE VARIO
015700040331     C                   IF        %SUBST(CSBTBL: 1: 1)='F'
015800040331     C                   EVAL      COMODO = 8888
015900040331     C                   MOVEL     CSBLNP        COMODO
016000040331     C                   ELSE
016100040331     C                   EVAL      COMODO = 9999
016200040331     C                   MOVEL     CSBLNA        COMODO
016300040331     C                   END
016400040331     C* aggiorno TNCSB00F
016500040331     C                   IF        CSBCDI<>COMODO OR CSBKSC<>COMODO
016600040331     C     KSPE          chain     TNCSB03l
016700040331     c                   if        %found(tncsB03l)
016800040331     C                   EVAL      CSBCDI = COMODO
016900040331     C                   EVAL      CSBKSC = COMODO
017000040331     C                   UPDATE    TNCSB000
017100040331     C                   END
017200040331     C                   END
017300040331     C                   ENDsr
017400040331     C**********************************************************************
017500040331     C     SRCLEAR       BEGSR
017600040331     C**********************************************************************
017700040331     C                   SETOFF                                       28
017800040331     C                   MOVEA     '000000000'   *IN(01)
017900040331     C                   Z-ADD     0             VLNP
018000040331     C                   Z-ADD     0             VNSP
018100040331     C                   Z-ADD     0             VAAS
018200040331     C                   Z-ADD     0             VNRS
018300040331     C                   MOVEL     *BLANKS       DESLNP
018400040331     C                   MOVEL     *BLANKS       VRSD
018500040331     C                   MOVEL     *BLANKS       VVID
018600040331     C                   MOVEL     *BLANKS       VCAD
018700040331     C                   MOVEL     *BLANKS       VCID
018800040331     C                   MOVEL     *BLANKS       VPRM
018900040331     C                   MOVEL     *BLANKS       VNZM
019000040331     C                   ENDSR
019100040331     C**********************************************************************
019200040331     C     VISBOL        BEGSR
019300040331     C**********************************************************************
019400990728     C                   CLEAR                   TNSB50
019500990728     C                   MOVEL     'C01'         I50OP0
019600040331     C                   MOVEL(P)  TNSB50        KPJBU
019700990728     C                   CALL      'TNSB50R'
019800941104     C                   PARM                    KPJBA
019900990728     C                   MOVEL     KPJBU         TNSB50
020000990728     C                   MOVE      D50AAS        VAAS
020100040331     C                   MOVE      D50LNP        VLNP
020200990728     C                   MOVE      D50NRS        VNRS
020300040331     C                   MOVE      D50NSP        VNSP
020400040331     C                   ENDSR
020500040331     C**********************************************************************
020600040331     C     VISCAS        BEGSR
020700040331     C**********************************************************************
020800040331     C                   CLEAR                   KPJBU
020900951211     C                   CALL      'TNSC40R'
021000941107     C                   PARM                    KPJBA
021100040331     C                   ENDSR
021200040331     C**********************************************************************
021300020130     C     *INZSR        BEGSR
021400040331     C**********************************************************************
021500941028     C     *ENTRY        PLIST
021600941028     C                   PARM                    KPJBA
021700941028     C                   Z-ADD     1             CODUT
021800941028     C                   CALL      'XPARUT'
021900941028     C                   PARM                    UTEDSE
022000941028     C                   MOVEL     RAGUT         RSUT             20
022100941028     C                   MOVEL     RAGUT         RSUT2            25
022200941028     C*---------------------------------------------------------------*
022300941028     C* GIRO UDATE
022400941028     C                   CLEAR                   WLBDA8
022500941028     C                   TIME                    WHHDAT           14 0
022600941028     C                   MOVE      WHHDAT        G02DAT
022700941028     C                   CALL      'XSRDA8'
022800941028     C                   PARM                    WLBDA8
022900941028     C                   Z-ADD     G02INV        DATEU             8 0
023000941028     C*
023100941028     C* DEFINIZIONE CHIAVI
023200040331     C     kspe          KLIST
023300040331     C                   KFLD                    VAAS
023400040331     C                   KFLD                    vlnp
023500040331     C                   KFLD                    VNRS
023600040331     C                   KFLD                    vnsp
023700941028     C*
023800941028     C                   ENDSR
023900971218**   SK - SCHIERE PER MESSAGGI
024000941028Linea di partenza inesistente                                                 03
024100941028Numero di spedizione non valido                                               04
024200941028Anno di spedizione errato                                                     09
024300040331Campo obbligatorio o errato                                                   12
024400040401Contrassegno inesistente o già pagato                                         12
