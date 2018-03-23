000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*----------------------------------------------------------------H
000300040331     FTNCSB03L  UF   E           K DISK
000400040331     FTNCSA01L  UF a E           K DISK
000500040331     Fazorg01L  iF   E           K DISK
000600120213     FTNCSb09L  iF   E           K DISK    rename(tncsb000: tncsb009)
000700120227     f                                     prefix(£)
000800040331     FTNSC01D   CF   E             WORKSTN
000900941028     F*----------------------------------------------------------------H
001000011113     D                SDS
001100011113     D NomPgm            *PROC
001200120213     D SK              S             78    DIM(6) CTDATA PERRCD(1)
001300040331     D COMODO          S              7  0 inz
001400941028     D*----------------------------------------------------------------
001500040331     D WLBDA8          DS
001600040331     D  G02DAT                 1      8  0
001700040331     D  G02INV                 9     16  0
001800040331     D  G02ERR                17     17
001900040331     D  G02TGI                18     22  0
002000120213     D comkey          s                   like(csbnra)
002100120213     D tnsc30ds      E DS
002200040331     D TNSB50        E DS                  EXTNAME(TNSB50DS)
002300000000     D KPJBA         E DS
002400000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002500000000     C*---------------------------------------------------------------*
002600040331     C                   EXSR      SRCLEAR
002700020130     c*
002800020131     C* emette il primo video solo se non sono chiamato dal pgm TIBS20R
002900040331     C                   DO        *HIVAL
003000040331     C                   EXFMT     SC01D01
003100120213     c* forzatura se multiassegno
003200120213     c                   if        *in44 and *inkf
003300120213     C                   SETON                                        28
003400120213     C                   MOVEL     SK(6)         $MSG
003500120213     C                   iter
003600120213     c                   end
003700040331     C   KC              LEAVE
003800120213     C   KG              EXSR      VISCAS
003900040331     C   KH              EXSR      VISBOL
004000120213     C   Ki              EXSR      multiass
004100040331     C                   EXSR      SRCTR
004200040331     C   28              ITER
004300120227     C* CODICE CLIENTE VARIO
004400120227     c* in caso di multiassegno il codice cliente saraà quello della
004500120227     c* spedizione richiesta a video per tutte le spedizioni legate
004600120227     C                   IF        %SUBST(CSBTBL: 1: 1)='F'
004700120227     C                   EVAL      COMODO = 8888
004800120227     C                   MOVEL     CSBLNP        COMODO
004900120227     C                   ELSE
005000120227     C                   EVAL      COMODO = 9999
005100120227     C                   MOVEL     CSBLNA        COMODO
005200120227     C                   END
005300120227     c*
005400120213     c                   if        *inkf or *inkj
005500120220     c                   if        %subst(csbnra: 10: 1)<> ' ' and
005600120220     c                             csbtpi = 'M' and csbabi = 0
005700120213     C                   EXSR      SRAGGIO1
005800120213     c                   else
005900120213     C                   EXSR      SRAGGIO
006000120213     c                   end
006100040331     C                   EXSR      SRclear
006200040331     c                   end
006300040331     C                   ENDDO
006400040331     C                   EVAL      *INLR = *ON
006500040331     C**********************************************************************
006600040331     C     SRCTR         BEGSR
006700040331     C**********************************************************************
006800120213     C                   SETOFF                                       28
006900040331     C                   MOVEA     '000000000'   *IN(01)
007000040331     c                   do
007100040331     C     vaas          IFEQ      0
007200040331     c                   seton                                        2801
007300040331     c                   movel     sk(3)         $msg
007400040331     c                   leave
007500040331     c                   end
007600941028     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
007700941028     C                   MOVEL     VAAS          WAA2              2 0
007800941028     C     WAA2          IFEQ      0
007900941028     C     VAAS          IFGT      60
008000941028     C                   ADD       1900          VAAS
008100941028     C                   ELSE
008200941028     C                   ADD       2000          VAAS
008300941028     C                   END
008400941028     C                   END
008500000000     C*
008600900420     C* LINEA DI PARTENZA DEVE ESISTERE SU AZORG
008700040331     C     vlnp          CHAIN     AZORG01L                           02
008800040331     C     *IN02         IFEQ      '1'
008900040331     C                   SETON                                        28
009000040331     C                   MOVEL     SK(1)         $MSG
009100040331     C                   leave
009200941028     C                   END
009300040331     C                   MOVEL     ORGDES        DESlnp
009400000000     C*
009500000000     C* NUMERO DI SPEDIZIONE
009600040331     C     vnsp          IFEQ      0
009700040331     C                   SETON                                        2803
009800040331     C                   MOVEL     SK(2)         $MSG
009900040331     C                   leave
010000941028     C                   END
010100000000     C*
010200120213     C* SE CONTRASSEGNO non ESISTE SU TNCSB ERRORE
010300000000     C*
010400040331     C     KSPE          chain     TNCSB03l
010500040401     C                   IF        not %found(tncsb03l) or
010600040401     C                             (%found(tncsb03l) and csbddp <> 0)
010700040331     C                   SETON                                        280102
010800040331     C                   SETON                                        03
010900040331     C                   MOVEL     SK(5)         $MSG
011000040331     C                   leave
011100040331     C                   ELSE
011200120213     c* multiassegno
011300120220     c                   if        %subst(csbnra: 10: 1)<> ' ' and
011400120220     c                             csbtpi = 'M' and csbabi = 0
011500120213     c                   seton                                        44
011600120213     c                   eval      comkey = csbnra
011700120213     c                   else
011800120214     C                   CLEAR                   COMKEY
011900120213     c                   setoff                                       44
012000120213     c                   end
012100120213     c*
012200040331     C     KSPE          chain     TNCSA01l
012300040331     C                   IF        %FOUND(TNCSA01L)
012400040331     C     vrsd          IFEQ      *BLANKS
012500040331     C                   eval      VRSD =        CSARSD
012600040331     C                   END
012700040331     C     vvid          ifEQ      *BLANKS
012800040331     C                   eval      VVID =        CSAVID
012900040331     C                   END
013000040331     C     vcad          ifEQ      *BLANKS
013100040331     C                   eval      VCAD =        CSACAD
013200040331     C                   END
013300040331     C     vcid          ifEQ      *BLANKS
013400040331     C                   eval      VCID =        CSACID
013500040331     C                   END
013600040331     C     vprm          ifEQ      *BLANKS
013700040331     C                   eval      VPRM =        CSAPRM
013800040331     C                   END
013900040331     C     vnzm          ifEQ      *BLANKS
014000040331     C                   eval      VNZM =        CSANZM
014100941031     C                   END
014200040331     C                   END
014300040331     C                   END
014400040331     C* riferimenti mittente
014500040331     C     vrsd          IFEQ      *BLANKS
014600040331     C                   SETON                                        2804
014700040331     C                   MOVEL     SK(04)        $MSG
014800040331     C                   leave
014900000000     C                   END
015000040331     C     vvid          ifEQ      *BLANKS
015100040331     C                   SETON                                        2805
015200040331     C                   MOVEL     SK(04)        $MSG
015300040331     C                   leave
015400040331     C                   END
015500040331     C     vcad          ifEQ      *BLANKS
015600040331     C                   SETON                                        2806
015700040331     C                   MOVEL     SK(04)        $MSG
015800040331     C                   leave
015900040331     C                   END
016000040331     C     vcid          ifEQ      *BLANKS
016100040331     C                   SETON                                        2807
016200040331     C                   MOVEL     SK(04)        $MSG
016300040331     C                   leave
016400040331     C                   END
016500040331     C     vprm          ifEQ      *BLANKS
016600040331     C                   SETON                                        2808
016700040331     C                   MOVEL     SK(04)        $MSG
016800040331     C                   leave
016900941031     C                   END
017000040331     c                   enddo
017100040331     c                   endsr
017200120213     c**********************************************************************
017300120213     c     sraggio1      begsr
017400120213     c**********************************************************************
017500120213     C     kcsb          SETLL     tncsb09l
017600120213?    C                   do        *hival
017700120213     C     kcsb          READE     tncsb09l
017800120213     C                   if        %eof(tncsb09l)
017900120213     c                   leave
018000120213     c                   end
018100120213     c* solo il contrassegno con legame
018200120227     c                   if        £csbnra <> comkey
018300120213     c                   iter
018400120213     c                   end
018500120227     c                   eval      VAAS  =      £CSbAAS
018600120227     c                   eval      VLNP  =      £CSbLNP
018700120227     c                   eval      VNSP  =      £CSbNSP
018800120227     c                   eval      VNRS  =      £CSbNRS
018900120213     C*
019000120213?    C                   EXSR      sraggio
019100120213     C                   enddo
019200120213     c                   endsr
019300040331     c**********************************************************************
019400040331     c     sraggio       begsr
019500040331     c**********************************************************************
019600040331     C* CREO TNCSA00F
019700040331     C     KSPE          chain     TNCSA01l
019800040331     C                   Z-ADD     VAAS          CSAAAS
019900040331     C                   Z-ADD     VLNP          CSALNP
020000040331     C                   Z-ADD     VNSP          CSANSP
020100040331     C                   Z-ADD     VNRS          CSANRS
020200040331     C                   MOVEL     VRSD          CSARSD
020300040331     C                   MOVEL     VVID          CSAVID
020400040331     C                   MOVEL     VCAD          CSACAD
020500040331     C                   MOVEL     VCID          CSACID
020600040331     C                   MOVEL     VPRM          CSAPRM
020700040331     C                   MOVEL     VNZM          CSANZM
020800000000     C*
020900040331     c                   if        not %found(tncsa01l)
021000941028     C                   WRITE     TNCSA000
021100040331     c                   else
021200040331     C                   update    TNCSA000
021300040331     c                   end
021400040331     C* aggiorno TNCSB00F
021500040331     C     KSPE          chain     TNCSB03l
021600120227     c                   if        %found(tncsB03l) and
021700120227     C                             (CSBCDI<>COMODO OR CSBKSC<>COMODO)
021800040331     C                   EVAL      CSBCDI = COMODO
021900040331     C                   EVAL      CSBKSC = COMODO
022000040331     C                   UPDATE    TNCSB000
022100040331     C                   END
022200040331     C                   ENDsr
022300040331     C**********************************************************************
022400040331     C     SRCLEAR       BEGSR
022500040331     C**********************************************************************
022600120213     C                   SETOFF                                       2844
022700040331     C                   MOVEA     '000000000'   *IN(01)
022800040331     C                   Z-ADD     0             VLNP
022900040331     C                   Z-ADD     0             VNSP
023000040331     C                   Z-ADD     0             VAAS
023100040331     C                   Z-ADD     0             VNRS
023200040331     C                   MOVEL     *BLANKS       DESLNP
023300040331     C                   MOVEL     *BLANKS       VRSD
023400040331     C                   MOVEL     *BLANKS       VVID
023500040331     C                   MOVEL     *BLANKS       VCAD
023600040331     C                   MOVEL     *BLANKS       VCID
023700040331     C                   MOVEL     *BLANKS       VPRM
023800040331     C                   MOVEL     *BLANKS       VNZM
023900040331     C                   ENDSR
024000040331     C**********************************************************************
024100040331     C     VISBOL        BEGSR
024200040331     C**********************************************************************
024300990728     C                   CLEAR                   TNSB50
024400990728     C                   MOVEL     'C01'         I50OP0
024500040331     C                   MOVEL(P)  TNSB50        KPJBU
024600990728     C                   CALL      'TNSB50R'
024700941104     C                   PARM                    KPJBA
024800990728     C                   MOVEL     KPJBU         TNSB50
024900990728     C                   MOVE      D50AAS        VAAS
025000040331     C                   MOVE      D50LNP        VLNP
025100990728     C                   MOVE      D50NRS        VNRS
025200040331     C                   MOVE      D50NSP        VNSP
025300040331     C                   ENDSR
025400120213     C**********************************************************************
025500120213     C     multiass      BEGSR
025600120213     C**********************************************************************
025700120213     c                   clear                   tnsc30ds
025800120213     c                   eval      sc30CMTI = 'N'
025900120213     c                   eval      sc30AMBI = 'S'
026000120213     c                   eval      sc30TPCI = 'V'
026100120213     c                   eval      sc30AASI = VAAS
026200120213     c                   eval      sc30LNPI = VLNP
026300120213     c                   eval      sc30NRSI = VNRS
026400120213     c                   eval      sc30NSPI = VNSP
026500120213     c                   movel(p)  tnsc30ds      kpjbu
026600120213     c                   call      'TNSC30R'
026700120213     c                   parm                    kpjba
026800120213     C                   ENDSR
026900040331     C**********************************************************************
027000040331     C     VISCAS        BEGSR
027100040331     C**********************************************************************
027200040331     C                   CLEAR                   KPJBU
027300951211     C                   CALL      'TNSC40R'
027400941107     C                   PARM                    KPJBA
027500040331     C                   ENDSR
027600040331     C**********************************************************************
027700020130     C     *INZSR        BEGSR
027800040331     C**********************************************************************
027900941028     C     *ENTRY        PLIST
028000941028     C                   PARM                    KPJBA
028100941028     C                   Z-ADD     1             CODUT
028200941028     C                   CALL      'XPARUT'
028300941028     C                   PARM                    UTEDSE
028400941028     C                   MOVEL     RAGUT         RSUT             20
028500941028     C                   MOVEL     RAGUT         RSUT2            25
028600941028     C*---------------------------------------------------------------*
028700941028     C* GIRO UDATE
028800941028     C                   CLEAR                   WLBDA8
028900941028     C                   TIME                    WHHDAT           14 0
029000941028     C                   MOVE      WHHDAT        G02DAT
029100941028     C                   CALL      'XSRDA8'
029200941028     C                   PARM                    WLBDA8
029300941028     C                   Z-ADD     G02INV        DATEU             8 0
029400941028     C*
029500941028     C* DEFINIZIONE CHIAVI
029600040331     C     kspe          KLIST
029700040331     C                   KFLD                    VAAS
029800040331     C                   KFLD                    vlnp
029900040331     C                   KFLD                    VNRS
030000040331     C                   KFLD                    vnsp
030100120213     c     kcsb          klist
030200120213     c                   kfld                    csbrgf
030300120213     c                   kfld                    csbndt
030400120213     c                   kfld                    csbddc
030500941028     C*
030600941028     C                   ENDSR
030700971218**   SK - SCHIERE PER MESSAGGI
030800120213Linea di partenza inesistente                                                 01
030900120213Numero di spedizione non valido                                               02
031000120213Anno di spedizione errato                                                     03
031100120213Campo obbligatorio o errato                                                   04
031200120213Contrassegno inesistente o già pagato                                         05
031300120214Sono presenti altre spedizioni/assegni. F10 per forzare                       06
