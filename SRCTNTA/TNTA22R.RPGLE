000100011015     H DECEDIT('0,') DATEDIT(*DMY.)
000200011015      *****************************************************************
000300011015      *                                                               *
000400011015      *         STAMPA MANDATI VARIATI  ( BATCH )                     *
000500011015      *                                                               *
000600011015      *****************************************************************
000700011015
000800011015     FTITPT01L  IP   E           K DISK
000900011015     FTNTAM01L  IF   E           K DISK
001000011015     FTABEL00F  IF   E           K DISK
001100011015     FTNTA22P   O    E             PRINTER oflind(*IN01)
001200011015
001300011015      *****************************************************************
001400011015
001500011015      *   S C H I E R E
001600011015
001700011015     D TMA             s              1    dim(1) ctdata perrcd(1)
001800011015     D TMAD            s             10    dim(1) alt(TMA)
001900011016     D APL             s              1    dim(2) ctdata perrcd(1)
002000011016     D APLD            s             10    dim(2) alt(APL)
002100011016     D*
002200011016     D* Max 99 codici consegna particolari !!!
002300011016     D FTC             s                   dim(99) like(TPTftc)
002400011015
002500011015      *   D S   I N T E R N E / E S T E R N E
002600011015
002700011015     D Parm22          DS
002800011015     D  ParDal                 1      8  0
002900011015     D  ParAl                  9     16  0
003000011016
003100011016     D G02DS           DS
003200011016     D  G02DAT                 1      8  0
003300011016     D  G02INV                 9     16  0
003400011016     D  G02ERR                17     17
003500011016     D  G02TGI                18     22  0
003600011015
003700011015     D KPJBA         E DS
003800011016     D UT§DSE0F      E DS
003900011016     D DS1P          E DS
004000011016     D DSTR          E DS
004100011016     D DSTA01        E DS
004200011017     D TIBS69DS      E DS                  INZ
004300011017     D DS_cnaco      E DS                  extname(CNACO00F) INZ
004400011017     D DS_cnind      E DS                  extname(CNIND00F) INZ
004500011017     D DS_cnclp      E DS                  extname(CNCLP00F) INZ
004600011017     D DS_fncls      E DS                  extname(FNCLS00F) INZ
004700011015
004800011015      *   V A R I A B I L I
004900011015
005000011016     D Ok_Rec          S              1
005100011015     D X               S              3  0
005200011016     D wpoTPT          S              3    inz
005300011016     D wpoACO          S              3    inz
005400011015
005500011015      *****************************************************************
005600011016      * RIEPILOGO INDICATORI
005700011016      *****************************************************************
005800011016      * 01    - OVERFLOW
005900011016      * 30    - DI COMODO
006000011016      *****************************************************************
006100011015
006200011016     C                   exsr      Sr_Seleziona
006300011016      *
006400011016IF  1C                   if        Ok_Rec = *on
006500011016      *
006600011015     C                   clear                   DETTA
006700011015      *
006800011015     C                   exsr      Sr_Decodifica
006900011015      * Testata
007000011016IF  2C                   if        *in01
007100011016     C                   write     testa1
007200011016     C                   write     testa2
007300011015     C                   eval      *in01 = *off
007400011016E   2C                   endif
007500011015      * Dettaglio
007600011015     C                   write     detta                                01
007700011016      *
007800011016E   1C                   endif
007900011016      *
008000011017     CLR                 exsr      Sr_Fine
008100011016
008200011016      *---------------------------------------------------------------*
008300011016      * SELEZIONA
008400011016      *---------------------------------------------------------------*
008500011016     C     Sr_Seleziona  BEGSR
008600011016
008700011016     C                   eval      Ok_Rec =  *on
008800011016     C*
008900011016S   1C                   select
009000011016    1C                   when      TPTatb = 'A'
009100011016     C                   eval      Ok_Rec =  *off
009200011016    1C                   when      TPTduv <  PARdal   or
009300011016     C                             TPTduv >  PARal
009400011016     C                   eval      Ok_Rec =  *off
009500011016X   1C                   other
009600011016     C     TPTftc        lookup    FTC                                    30
009700011016IF  2C                   if        not *in30
009800011016     C                   eval      Ok_Rec =  *off
009900011016E   2C                   endif
010000011016E   1C                   endsl
010100011016
010200011017     C                   ENDSR
010300011016
010400011015
010500011015      *---------------------------------------------------------------*
010600011015      * DECODIFICA
010700011015      *---------------------------------------------------------------*
010800011015     C     Sr_Decodifica BEGSR
010900011015
011000011015     C* aggancio file TNTAM
011100011016     C     ktntam        chain     TNTAM01L
011200011016IF  1C                   if        not %found(tntam01l)  or
011300011015     C                             TAMatb = 'A'
011400011015     C                   clear                   TAMddt
011500011015     C                   clear                   TAMdst
011600011015     C                   clear                   TAMflo
011700011015     C                   clear                   DSTA01
011800011015     C                   else
011900011015     C                   movel     TAMflo        DSTA01
012000011016E   1C                   endif
012100011015
012200011016     C* Cambio P.O. (per salto pagina)
012300011016     C                   movel     TPTksc        wpoTPT
012400011017     C*** (più sotto...) movel     I69ksc        wpoACO
012500011016IF  1C                   if        wpoTPT <> wpoACO
012600011016     C                   movel     *on           *in01
012700011016E   1C                   endif
012800011016
012900011015     C* Cliente
013000011017     C                   z-add     TPTksc        DKSC
013100011017IF  1C                   if        TPTksc <> I69kac
013200011017     C                   clear                   DS_cnaco
013300011017     C                   reset                   TIBS69DS
013400011017     C                   z-add     TPTksc        I69kac
013500011017     C                   call      'TIBS69R'
013600011017     C                   parm                    tibs69DS
013700011017     C                   parm                    DS_cnaco
013800011017     C                   parm                    DS_cnind
013900011017     C                   parm                    DS_cnclp
014000011017     C                   parm                    DS_fncls
014100011017IF  2C                   if        O69err <> *on
014200011015     C                   movel     ACOrag        DKSCD
014300011016E   2C                   else
014400011016     C                   movel     *all'? '      DKSCD
014500011016E   2C                   endif
014600011017     C                   movel     TPTksc        wpoACO
014700011016E   1C                   endif
014800011015
014900011015     C* Codice tariffa
015000011015     C                   movel     TPTctr        DCTR
015100011015
015200011015     C* Progressivo tariffa
015300011015     C                   movel     TPTprg        DPRG
015400011015
015500011015     C* Data decorrenza
015600011016IF  1C                   if        TAMddt <> 0
015700011016     C                   clear                   G02DS
015800011016     C                   z-add     TAMddt        G02INV
015900011015     C                   movel     '3'           G02ERR
016000011015     C                   call      'XSRDA8'
016100011015     C                   parm                    G02DS
016200011016     C                   z-add     G02DAT        DDDT
016300011016E   1C                   endif
016400011015
016500011015     C* Data scadenza
016600011016IF  1C                   if        TAMdst <> 0
016700011016     C                   clear                   G02DS
016800011016     C                   z-add     TAMdst        G02INV
016900011015     C                   movel     '3'           G02ERR
017000011015     C                   call      'XSRDA8'
017100011015     C                   parm                    G02DS
017200011016     C                   z-add     G02DAT        DDST
017300011016E   1C                   endif
017400011015
017500011015     C* Codice tariffa particolare
017600011015     C                   movel     TPTftc        DFTC
017700011015
017800011015     C* Data ultima variazione della tariffa particolare
017900011016IF  1C                   if        TPTduv <> 0
018000011016     C                   clear                   G02DS
018100011016     C                   z-add     TPTduv        G02INV
018200011015     C                   movel     '3'           G02ERR
018300011015     C                   call      'XSRDA8'
018400011015     C                   parm                    G02DS
018500011016     C                   z-add     G02DAT        DDUV
018600011016E   1C                   endif
018700011015
018800011015     C* Tipo tariffa particolare
018900011015     C                   movel     TPTtpg        DTPG
019000011015     C                   movel     'TR'          kcod
019100011015     C                   movel(P)  TPTtpg        kkey
019200011015     C     ktab          chain     tabel00f
019300011016IF  1C                   if        %found(tabel00f)
019400011015     C                   movel     TBLuni        DSTR
019500011015     C                   movel     §TRdes        DTPGD
019600011016E   1C                   endif
019700011015
019800011015     C* Importo
019900011015     C                   z-add     TPTvlm        DVLM
020000011015
020100011015     C* Divisa
020200011016IF  1C                   if        TPTvlm <> 0
020300011015     C                   movel     §TAdiv        DVVM
020400011016E   1C                   endif
020500011015
020600011015     C* Tipo valore merce
020700011015     C                   movel     TPTfvm        DFVM
020800011016     C                   movel     'TR'          kcod
020900011015     C                   movel(P)  TPTfvm        kkey
021000011015     C     ktab          chain     tabel00f
021100011016IF  1C                   if        %found(tabel00f)
021200011015     C                   movel     TBLuni        DSTR
021300011015     C                   movel     §TRdes        DFVMD
021400011016E   1C                   endif
021500011015
021600011015     C* Tipo mandato
021700011015     C                   eval      X = 1
021800011016     C     TPTtma        lookup    TMA(X)                                 30
021900011016IF  1C                   if        *in30
022000011016     C                   movel     TMAD(X)       DTMAD
022100011016E   1C                   endif
022200011015
022300011015     C* Cod. applicazione
022400011015     C                   eval      X = 1
022500011016     C     TPTapl        lookup    APL(X)                                 30
022600011016IF  1C                   if        *in30
022700011016     C                   movel     APLD(X)       DAPLD
022800011016E   1C                   endif
022900011015
023000011017     C                   ENDSR
023100011016
023200011015      *---------------------------------------------------------------*
023300011015      * ROUTINE INIZIALE
023400011015      *---------------------------------------------------------------*
023500011015     C     *INZSR        BEGSR
023600011015
023700011015     C     *ENTRY        PLIST
023800011015     C                   PARM                    KPJBA
023900011015     C                   movel     kpjbu         Parm22
024000011015      *
024100011015     C                   Z-ADD     1             CODUT
024200011015     C                   CALL      'X§PARUT'
024300011015     C                   PARM                    UT§DSE0F
024400011015     C                   MOVEL     RAGUT         RSUT
024500011015      *
024600011016IF  1C                   if        ParDal <> 0
024700011016     C                   clear                   G02DS
024800011016     C                   z-add     ParDal        G02INV
024900011015     C                   movel     '3'           G02ERR
025000011015     C                   call      'XSRDA8'
025100011015     C                   parm                    G02DS
025200011016     C                   z-add     G02DAT        TDTADA
025300011016E   1C                   endif
025400011016IF  1C                   if        ParAl  <> 0
025500011016     C                   clear                   G02DS
025600011016     C                   z-add     ParAl         G02INV
025700011015     C                   movel     '3'           G02ERR
025800011015     C                   call      'XSRDA8'
025900011015     C                   parm                    G02DS
026000011016     C                   z-add     G02DAT        TDTAA
026100011016E   1C                   endif
026200011015      *
026300011015     C                   movel     *date         WDATE
026400011016      *
026500011016      * intabello le tariffe particolari da considerare
026600011016      * e stampo i parametri (comprese tali tariffe)
026700011016     C                   write     testa1
026800011016     C                   write     param1
026900011016     C                   movel     *off          *in01
027000011016     C                   clear                   X
027100011016     C                   clear                   FTC
027200011016     C                   movel     '1P'          kcod
027300011016     C     ktab2         setll     TABEL00F
027400011016     C     ktab2         reade     TABEL00F
027500011016     C*
027600011016IF  1C                   dow       not %eof(tabel00f)
027700011016     C                   movel     TBLuni        DS1P
027800011016     C*
027900011016IF  2C                   if        §1Ptps = 'S'
028000011016     C                   add       1             X
028100011016     C                   movel     TBLkey        FTC(X)
028200011016     C*
028300011016IF  2C                   if        *in01
028400011016     C                   write     testa1
028500011016     C                   write     param1
028600011016     C                   eval      *in01 = *off
028700011016E   2C                   endif
028800011016     C                   movel     TBLkey        Pftc
028900011016     C                   movel     §1Pdes        PftcD
029000011016     C                   write     param2                               01
029100011016     C*
029200011016IF  3C                   if        X = 99
029300011016     C                   leave
029400011016E   3C                   endif
029500011016     C*
029600011016E   2C                   endif
029700011016     C     ktab2         reade     TABEL00F
029800011016     C*
029900011016E   1C                   enddo
030000011015
030100011016     C* file TNTAM01L
030200011015     C     ktntam        KLIST
030300011016     C                   KFLD                    TPTksc
030400011016     C                   KFLD                    TPTctr
030500011016     C                   KFLD                    TPTprg
030600011016     C* file TABEL00F
030700011016     C     ktab          KLIST
030800011016     C                   KFLD                    codut
030900011016     C                   KFLD                    kcod
031000011016     C                   KFLD                    kkey
031100011016     C     ktab2         KLIST
031200011016     C                   KFLD                    codut
031300011016     C                   KFLD                    kcod
031400011016
031500011016     C     *like         define    TBLcod        kcod
031600011016     C     *like         define    TBLkey        kkey
031700011015
031800011017     C                   ENDSR
031900011017
032000011017      *---------------------------------------------------------------*
032100011017      * ROUTINE FINALE
032200011017      *---------------------------------------------------------------*
032300011017     C     Sr_Fine       BEGSR
032400011017
032500011017     C                   write     fine
032600011017      *
032700011017     C                   reset                   TIBS69DS
032800011017     C                   eval      I69TLA  = 'C'
032900011017     C                   call      'TIBS69R'
033000011017     C                   parm                    TIBS69DS
033100011017
033200011017     C                   ENDSR
033300011017      *
033400011015**  TMA / TMAD
033500011015FFittizio
033600011015**  APL / APLD
033700011015AArrivo
033800011015PPartenza
