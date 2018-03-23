000100960605      ***********************************************************************
000200960605      *   IMPORTA BOLLE DA PARTNER ESTERI            - E.D.I.   ESTERO -    *
000300960605      ***********************************************************************
000400960605     H DECEDIT('0,') DATEDIT(*DMY.)
000500000307      *---------------------------------------------------------------------*
000600960607     FEDTAB01L  UF A E           K DISK
000700120502     Fedstbl01l IF   E           K DISK
000800960611     FTABEL00F  IF   E           K DISK
000900960611     FCNACO00F  IF   E           K DISK
001000110914     FCNCLP00F  IF   E           K DISK
001100960607     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001200130118     FTRTC93D   CF   E             WORKSTN usropn
001300051209      *---------------------------------------------------------------*
001400130424     D PSIG            S              2    DIM(200)                             Sigle Utilizzate
001500130424     D CSIG            S             31    DIM(200)                             Cod PT parziale
001600051209     D NSIG            S              3  0
001700070215     D Nlnp            S              3  0
001800130424     D PTlnp           S              3  0 DIM(200)                             tutte le linee
001900130424     D PR_DS           S                   like(dsPR) DIM(200)
002000130424     D PR_key          S              6               DIM(200)
002100130424     D PI_DS           S                   like(dsPI) DIM(200)
002200130424     D PI_key          S              6               DIM(200)
002300130424     D PF_DS           S                   like(dsPF) DIM(200)
002400130424     D PF_key          S              6               DIM(200)
002500111221      *-------------
002600111221     D* Parametri per pgm gestione STATUS alternativi Partner
002700111221     D  TABDS          DS
002800111221     D  XTAOPZ                 1      2
002900111221     D  XTARET                 3      3
003000111221     D  XTAOPR                 4      4
003100111221     D  XTAERR                 5      5
003200111221     D  XTAKEY1                6      8
003300111221     D  XTAKEY2                9     11
003400111221     D  XTACOD1               12     14
003500111221     D  XTACOD2               15     17
003600111221     D  XTAUNBcli             18     52
003700111221     D  XTARAGs               53     84
003800000307      *---------------------------------------------------------------*
003900960611     D C1B             S              2    DIM(200)                             Cod.tratt.merce
004000960611     D D1B             S             89    DIM(200)                             Cod.tratt.merce
004100011218     D C15             S              3    DIM(500)                             Cod.Nazione
004200011218     D D15             S             25    DIM(500)                             Decodifica
004300160205     D ERR             S             70    DIM(35) CTDATA PERRCD(1)             Errori
004400130509     D Operazione      S              1
004500000307      *---------------------------------------------------------------*
004600960605     D KPJBA         E DS
004700110819     D kPJBuS          s                   like(KPJBU)
004800000307     D CNCR80        E DS
004900120416     D DS3k          E DS
005000000307     D DS1B          E DS
005100000307     D DSPT          E DS                  EXTNAME(EDIDSPT)
005200000307     D DSPU          E DS                  EXTNAME(EDIDSPU)
005300070123     D DSPV          E DS                  EXTNAME(EDIDSPV)
005400101019     D DSPw          E DS                  EXTNAME(EDIDSPW)
005500101019     D DSPz          E DS                  EXTNAME(EDIDSPZ)
005600140221     D DSPs          E DS                  EXTNAME(EDIDSPS)
005700110913     D DSPn          E DS                  EXTNAME(EDIDSPN)
005800130412     D DSPo          E DS                  EXTNAME(EDIDSPO)
005900101019     D DSPR          E DS                  EXTNAME(EDIDSPR)
006000101019     D DSPF          E DS                  EXTNAME(EDIDSPF)
006100101019     D DSPI          E DS                  EXTNAME(EDIDSPI)
006200101019      ********
006300000307     D TRTC91        E DS                  EXTNAME(TRTC91DS)
006400000307     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006500000307     D  TCU                  398    697    DIM(50)                              PARAM FLG7/8,TIPC
006600000307     D  KCU                  698    847P 0 DIM(50)   PACKEVEN                   PARAM CAPI CONTO
006700100205     D H2cKSC          s                   like(V2cKSC)
006800130520     D savMUS          s                   like(KnMUS)
006900130520     D savSIF          s                   like(KnSIF)
007000000307      *---------------------------------------------------------------*
007100960611     D TCUDS           DS
007200960611     D  F34                    3      4
007300960611     D  F56                    5      6
007400960605     D                SDS
007500960605     D  NOMPGM                 1     10
007600080623      **
007700080623     D Digits          C                   '0123456789'
007800130509     D Annulla         C                   'A'
007900130509     D Ripristina      C                   'R'
008000000307      *----------------------------------------------------------------
008100960607     IAZORG01L  AA
008200960607     I                             P    4    5 0ORGFIL
008300960611     I                                  6    6  ORGFVA
008400130515     I                                  7    7  ORGFAG
008500960611     I                                 14   33  ORGDES
008600000307      *---------------------------------------------------------------*
008700130515      * Inizializzo prima videata
008800130515     C                   EXSR      INZ01
008900130515     C                   MOVEL     '1'           $GESD             1
009000130509      *
009100960605      * Loop gestione videate
009200960605     C     WFINE         DOWEQ     'N'
009300130509     C     $GESD         CASEQ     '1'           GESD01                         Sce.tabella
009400130509     C     $GESD         CASEQ     '2'           GESD02                         Param.anagr.
009500130509     C     $GESD         CASEQ     '3'           GESD03                         Import IFCSUM
009600130509     C     $GESD         CASEQ     '4'           GESD04                         Param.Export
009700130509     C     $GESD         CASEQ     '5'           GESD05                         Trasmissione
009800130509     C     $GESD         CASEQ     '6'           GESD06                         Import IFTSTA
009900130509     C     $GESD         CASEQ     '7'           GESD07                         DATE ARRIVO
010000130509     C     $GESD         CASEQ     '8'           GESD08                         SUM96A IMPORT
010100130509     C     $GESD         CASEQ     'P'           GESRiep
010200960605     C                   END
010300960605     C                   END
010400960605      * Fine Lavoro
010500960605     C     FINE          TAG
010600960605     C                   SETON                                        LR
010700130515     C*---------------------------------------------------------------*
010800130515     C*  INZ01: Inizializzo prima videata
010900130515     C*---------------------------------------------------------------*
011000130515     C     INZ01         BEGSR
011100130515     C*
011200130515     C                   MOVEL     *BLANKS       V1CKEY
011300130515     C                   MOVE      ' ?'          V1CKEY
011400130515     C                   MOVEL     NOMPGM        V1CPGM
011500130515     c                   movel     *blank        aggiorna_db       1
011600130520     C                   MOVEL     KNMUS         savMUS
011700130520     C                   MOVEL     KNSIF         savSIF
011800130515     C*
011900130515     C                   ENDSR
012000960606     C*---------------------------------------------------------------*
012100960606     C*  GESD01: Gestione prima videata
012200960606     C*---------------------------------------------------------------*
012300960606     C     GESD01        BEGSR
012400960606     C*
012500960611     C                   EXFMT     TC93D01
012600960606     C*  Fine Lavoro
012700960606     C     *INKC         IFEQ      '1'
012800960606     C                   MOVEL     'S'           WFINE
012900960606     C                   GOTO      FINVD1
013000960606     C                   END
013100130509     C*
013200960606     C*  Controllo se devo richiamare interrogazione
013300960611     C     '?'           SCAN      V1CKEY                                 32
013400960606     C     *IN32         IFEQ      '1'
013500960606     C                   CLEAR                   TRTC91
013600960611     C                   MOVEL     'PT'          D91COD
013700960606     C                   MOVEL     TRTC91        KPJBU
013800110422     C                   CALL      'TRTC9PTR'
013900960606     C                   PARM                    KPJBA
014000960607     C                   MOVEL     KPJBU         TRTC91
014100960611     C                   MOVEL     D91KEY        V1CKEY
014200960606     C                   GOTO      FINVD1
014300960606     C                   END
014400130515     C*
014500130515     C*  controlla la presenza della tabella PT e le altre
014600130515     C                   EXSR      CHECK_PT
014700960607     C*
014800960606     C                   EXSR      INZ02
014900960606     C*
015000960606     C     FINVD1        ENDSR
015100130515     C*---------------------------------------------------------------*
015200130515     C*  CHECK_TAB:    aggancia la tabella PT & C.
015300130515     C*---------------------------------------------------------------*
015400130515     C     CHECK_PT      BEGSR
015500130515     C*
015600130515     C                   SETOFF                                       01  02
015700130515     C                   SETOFF                                       04  06
015800130515      *
015900130515     c                   clear                   trovata_PT        1
016000130515     c                   clear                   trovata_PU        1
016100130515     c                   clear                   trovata_PV        1
016200130515     c                   clear                   trovata_PW        1
016300130515     c                   clear                   trovata_PZ        1
016400140221     c                   clear                   trovata_PS        1
016500130515     c                   clear                   trovata_PO        1
016600130515     c                   clear                   trovata_PN        1
016700130515      *-
016800130515     C                   clear                   DSPT
016900130515     C                   clear                   DSPU
017000130515     C                   clear                   DSPV
017100130515     C                   clear                   DSPW
017200130515     C                   clear                   DSPZ
017300140221     C                   clear                   DSPS
017400130515     C                   clear                   DSPO
017500130515     C                   clear                   DSPN
017600130515      *-
017700130515     C*  Aggancio TABELLA PT
017800130515     C                   MOVEL     'PT'          KCOD
017900130515     C                   MOVEL     V1CKEY        KKEY
018000130515     C                   MOVEL     KKEY          V2CKEY
018100130515     C     KTAB          CHAIN     EDTAB01L
018200130515     c                   if        not %Found(EDTAB01L)
018300130515      * inserimento
018400130515     C                   SETON                                        01
018500130520      ***
018600130520      ***  Inizializzo tutte le schermate
018700130520     c                   clear                   TC93D02
018800130520     c                   clear                   TC93D03
018900130520     c                   clear                   TC93D04
019000130520     c                   clear                   TC93D05
019100130520     c                   clear                   TC93D06
019200130520     c                   clear                   TC93D07
019300130520     c                   clear                   TC93D08
019400130520     c                   clear                   TC93PTN
019500130520     c                   clear                   TC93W01
019600130520     c                   clear                   TC93W02
019700130520     c                   clear                   TC93W03
019800130520     c                   clear                   TC93NOT
019900130520     C                   MOVEL     V1cKEY        V2CKEY
020000130520     C                   eval      KNMUS = savMUS
020100130520     C                   eval      KNSIF = savSIF
020200130520      ***
020300130515     c                   elseIf        %Found(EDTAB01L)
020400130515      * modifica
020500130515     c                   move      'S'           trovata_PT
020600130515     c                   movel     'S'           aggiorna_db       1
020700130515     C                   MOVEL     TABUNI        DSPT
020800130515     C     TABFLG        COMP      ' '                                0404
020900130515      *
021000130515     C*  Aggancio TABELLA PU
021100130515     C                   MOVEL     'PU'          KCOD
021200130515     C     KTAB          CHAIN     EDTAB01L
021300130515     c                   if        %Found(EDTAB01L)
021400130515     c                   move      'S'           trovata_PU
021500130515      * x aggiunta di un campo numerico
021600130515     C                   if        %subst(TABUNI:41:2) = *blank
021700130515     C                   eval      %subst(TABUNI:41:2) = '01'
021800130515     c                   end
021900130515     C                   if        %subst(TABUNI:44:2) = *blank
022000130515     C                   eval      %subst(TABUNI:44:2) = '00'
022100130515     c                   end
022200130515      *
022300130515     C                   MOVEL     TABUNI        DSPU
022400130515     c                   endIF
022500130515      *
022600130515     C*  Aggancio TABELLA PV
022700130515     C                   MOVEL     'PV'          KCOD
022800130515     C     KTAB          CHAIN     EDTAB01L
022900130515     c                   if        %Found(EDTAB01L)
023000130515     c                   move      'S'           trovata_PV
023100130515     C                   MOVEL     TABUNI        DSPV
023200130515     c                   end
023300130515      *
023400130515     C*  Aggancio TABELLA PW
023500130515     C                   MOVEL     'PW'          KCOD
023600130515     C     KTAB          CHAIN     EDTAB01L
023700130515     c                   if        %Found(EDTAB01L)
023800130515     C                   MOVEL     TABUNI        DSPW
023900130515     c                   move      'S'           trovata_PW
024000130515     c                   end
024100130515      *
024200130515     C*  Aggancio TABELLA PZ
024300130515     C                   MOVEL     'PZ'          KCOD
024400130515     C     KTAB          CHAIN     EDTAB01L
024500130515     c                   if        %Found(EDTAB01L)
024600130515     C                   MOVEL     TABUNI        DSPZ
024700130515     c                   move      'S'           trovata_PZ
024800130515     c                   end
024900140221      *
025000140221     C*  Aggancio TABELLA PS
025100140221     C                   MOVEL     'PS'          KCOD
025200140221     C     KTAB          CHAIN     EDTAB01L
025300140221     c                   if        %Found(EDTAB01L)
025400140221     C                   MOVEL     TABUNI        DSPS
025500140221     c                   move      'S'           trovata_PS
025600140221     c                   end
025700130515      *
025800140221     C*  Aggancio TABELLA PO
025900130515     C                   MOVEL     'PO'          KCOD
026000130515     C     KTAB          CHAIN     EDTAB01L
026100130515     c                   if        %Found(EDTAB01L)
026200130515     C                   MOVEL     TABUNI        DSPO
026300130515     c                   move      'S'           trovata_PO
026400130515     c                   end
026500130515      *
026600130515     C*  Aggancio TABELLA P1-2-3-4-5  delle righe NOTE
026700130515     c                   clear                   NOT001
026800130515     C                   MOVEL     'P1'          KCOD
026900130515     C     KTAB          chain     EDTAB01L
027000130515     c                   if        %Found(EDTAB01L)
027100130515     C                   MOVEL     TABUNI        DSPN
027200130515     c                   move      'S'           trovata_PN
027300130515     C  n02§PnNOTA       comp      *blank                             0202
027400130515     c                   eval      not001 = §PnNOTA
027500130515     C                   Z-ADD     §PnDTini      v2dadat
027600130515     c                   end
027700130515      ***
027800130515     C                   MOVEL     'P2'          KCOD
027900130515     C     KTAB          chain     EDTAB01L
028000130515     c                   if        %Found(EDTAB01L)
028100130515     C                   MOVEL     TABUNI        DSPN
028200130515     c                   move      'S'           trovata_PN
028300130515     C  n02§PnNOTA       comp      *blank                             0202
028400130515     c                   eval      not002 = §PnNOTA
028500130515     c                   end
028600130515      ***
028700130515     C                   MOVEL     'P3'          KCOD
028800130515     C     KTAB          chain     EDTAB01L
028900130515     c                   if        %Found(EDTAB01L)
029000130515     C                   MOVEL     TABUNI        DSPN
029100130515     c                   move      'S'           trovata_PN
029200130515     C  n02§PnNOTA       comp      *blank                             0202
029300130515     c                   eval      not003 = §PnNOTA
029400130515     c                   end
029500130515      ***
029600130515     C                   MOVEL     'P4'          KCOD
029700130515     C     KTAB          chain     EDTAB01L
029800130515     c                   if        %Found(EDTAB01L)
029900130515     C                   MOVEL     TABUNI        DSPN
030000130515     c                   move      'S'           trovata_PN
030100130515     C  n02§PnNOTA       comp      *blank                             0202
030200130515     c                   eval      not004 = §PnNOTA
030300130515     c                   end
030400130515      ***
030500130515     C                   MOVEL     'P5'          KCOD
030600130515     C     KTAB          chain     EDTAB01L
030700130515     c                   if        %Found(EDTAB01L)
030800130515     C                   MOVEL     TABUNI        DSPN
030900130515     c                   move      'S'           trovata_PN
031000130515     C  n02§PnNOTA       comp      *blank                             0202
031100130515     c                   eval      not005 = §PnNOTA
031200130515     c                   end
031300130515      ***
031400130515     C                   END
031500130515     C*
031600130515     C                   ENDSR
031700130515     C*---------------------------------------------------------------*
031800130515     C*  INZ02: Inizializzo seconda videata
031900130515     C*---------------------------------------------------------------*
032000130515     C     INZ02         BEGSR
032100130515     C*
032200130515     C                   SETOFF                                       404142
032300130515     C                   SETOFF                                       434445
032400130515     C                   SETOFF                                       464748
032500130515     C                   SETOFF                                       49  28
032600130515     C*
032700130515     c                   clear                   forza_sigla
032800130515     C                   MOVEL     '2'           $GESD             1
032900130515     C                   MOVEL     *BLANKS       $MSG
033000130515     C                   MOVEL     NOMPGM        V2CPGM
033100130515      *
033200130515     C                   MOVEL     *BLANKS       V2DKSC
033300130515     C                   MOVEL     *BLANKS       V2DLNP
033400130515     C                   MOVEL     *BLANKS       V2DNAZ
033500130515     C                   MOVEL     *BLANKS       V2Dexp
033600130515      *****
033700130515     c                   if          trovata_PT = 'S'
033800130515      **
033900130515      * salva x il confronto e vedere se modificato
034000130515     C                   Z-ADD     §PTKSC        V2CKSC
034100130515     C                   Z-ADD     §PTKSC        H2CKSC
034200130515     C                   move      §PTKSC        alfaKSC           7
034300130515     c                   setoff                                       88
034400130515     c                   if        alfaksc <> v2cFTP and V2Cftp <> *blank
034500130515     c                                               and V2Cftp <> *zeros
034600130515     c                   seton                                        88
034700130515     c                   end
034800130515      *
034900130515     C*  Decodifico codice cliente
035000130515     C                   Z-ADD     1             KKUT
035100130515     C                   Z-ADD     KCI           KKCC
035200130515     C                   Z-ADD     §PTKSC        KKSC
035300130515     C     KACO          CHAIN     CNACO00F
035400130515     c                   if        %Found(CNACO00F)
035500130515     C                   MOVEL     ACORAG        V2DKSC
035600130515     C     KACO          CHAIN     CNCLP00F
035700130515     c                   if        %Found(CNCLP00F)
035800130515     C                   MOVEL     CLPDUS        V2DULT
035900130515     C                   MOVEL     CLPDUS        byte2             2
036000130515     C                   MOVE      byte2         V2DULT
036100130515     C                   MOVE      CLPDUS        byte2
036200130515     C                   MOVEL     byte2         V2DULT
036300130515     c                   end
036400130515     c                   end
036500130515     C*  Decodifico LNP
036600130515     C                   Z-ADD     §PTLNP        V2CLNP
036700130515     C                   Z-ADD     V2CLNP        ORGFIL
036800130515     C     ORGFIL        CHAIN     AZORG01L
036900130515     c                   if        %Found(AZORG01L)
037000130515     C                   MOVEL     ORGDES        V2DLNP
037100130515     c                   end
037200130515     C                   Z-ADD     §PTNRS        V2CNRS
037300130515     C*  Decodifico Nazione
037400130515     C                   MOVEL     §PTNAZ        V2CNAZ
037500130515     C                   Z-ADD     1             X
037600130515     C     V2CNAZ        LOOKUP    C15(X)                                 30
037700130515     C   30              MOVEL     D15(X)        V2DNAZ
037800130515     C                   MOVEL     §PTCTM        V2CCTM
037900130515     C                   MOVEL     §PTCTR        V2CCTR
038000130515     C                   MOVEL     §PTPLT        V2CPLT
038100130515     C                   MOVEL     §PTLOC        V2CLOC
038200130515     C                   MOVEL     §PTMSQ        V2CMSQ
038300130515      ***
038400130515     c                   if          trovata_PU = 'S'
038500130515      **
038600130515     C                   MOVEL     §PUTPC        V2CTPC
038700130515     C                   MOVEL     §PUcompat     V2Compat
038800130515     C                   MOVEL     §PUBS1        V2CBS1
038900130515     C                   MOVEL     §puSIG        V2Csig
039000130515     C                   MOVEL     §puNOMFILE    V2Cnomfile
039100130515     C                   MOVEL     §puRES        V3CRES
039200130515     C                   MOVEL     §PUpln        V2Cpln
039300130515     c                   clear                   v2TRSM
039400130515     C                   if        §PUxxx ='S'
039500130515     c                   move      'ON'          v2TRSM
039600130515     c                   end
039700130515      ***
039800130515      * Orm Automatici: Cliente/Tariffa
039900130515     C                   MOVEL     *BLANKS       V2Oksc
040000130515     c                   if        §puORMaKSC <> *blank
040100130515     c     digits        check     §puORMaKSC
040200130515      **  se NON presenti campi NON numerici: (Ossia NON trovati)
040300130515     c                   iF        not %Found
040400130515     C                   MOVE      §puORMaKSC    V2Oksc
040500130515     c                   end
040600130515     c                   end
040700130515     C                   MOVEL     §puORMaCTR    V2Octr
040800130515      **
040900130515     c                   end
041000130515      **
041100130515     c                   if          trovata_PZ = 'S'
041200130515      *
041300130515      * codice di Export (per i Partner) per indirizzare le mails x disposizioni
041400130515      *  giacenze in modo differente da quelle collegate al codice di import x
041500130515      *  comunicare l'apertura di giacenza.
041600130515     c                   if        §PZkscEXP =  *blank
041700130515     C                   eval      §PZkscEXP = *zeros
041800130515     c                   end
041900130515     C                   move      §PZkscEXP     V2Cexp
042000130515     C                   MOVEL     §PZftpCOD     V2Cftp
042100130515     C*  Decodifico codice cliente x Export (solo per i Partner)
042200130515     c                   clear                   V2Dexp
042300130515     c                   if        V2CTPC = 'P'
042400130515     C                   Z-ADD     1             KKUT
042500130515     C                   Z-ADD     KCI           KKCC
042600130515     C                   move (p)  §PZkscEXP     KKSC
042700130515     C     KACO          CHAIN     CNACO00F
042800130515     c                   if        %Found(cnaco00f)
042900130515     c                   MOVEL     ACORAG        V2Dexp
043000130515     c                   end
043100130515     c                   end
043200130515      **
043300130515     c                   end
043400130515      **
043500160205     c                   if          trovata_PS = 'S'
043600130515      **
043700160205     c                   clear                   v2dfgs
043800160205     c                   if        §PSfgs = *blank
043900160205     C                   MOVEL     *all'0'       V2FGS
044000160205     c                   else
044100160205     C                   MOVEL     §PSfgs        V2FGS
044200160205     C                   Z-ADD     V2fgs         ORGFIL
044300160205     C     ORGFIL        CHAIN     AZORG01L
044400160205     c                   if        %Found(AZORG01L)
044500160205     C                   MOVEL     ORGDES        V2Dfgs
044600160205     c                   end
044700160205     c                   end
044800160205      **
044900160205     c                   end
045000160205      **
045100160205     c                   if          trovata_PV = 'S'
045200160205      **
045300130515     C                   MOVEL     §PVema        V8EMAI
045400130515     C                   MOVEL     §PVrff        V8cRFF
045500130515     C                   MOVEL     §PVrfs        V8cRFS
045600130515     C                   MOVEL     §PVrse        V8cRSE
045700130515     C                   MOVEL     §PVrcu        V8cRCU
045800130515     C                   MOVEL     §PVpes        V8cPES
045900130515     C                   MOVEL     §PVcol        V8cCOL
046000130515     C                   MOVEL     §PVsin        V8cSIN
046100130515     C                   MOVEL     §PVicn        V8cICN
046200130515     C                   MOVEL     §PV2in        V8c2in
046300130515     C                   MOVEL     §PVinv        V8cINV
046400130515     C                   MOVEL     §PVcniRMN     V8cCNI
046500130515      **
046600130515     c                   end
046700130515      **
046800130515     c                   endIF
046900130515      *****
047000130515     C                   ENDSR
047100960607     C*---------------------------------------------------------------*
047200960607     C*  GESD02: Gestione seconda videata
047300960607     C*---------------------------------------------------------------*
047400960607     C     GESD02        BEGSR
047500960607     C*
047600960611     C                   EXFMT     TC93D02
047700110913     C*
047800960607     C                   SETOFF                                       404142
047900960611     C                   SETOFF                                       434445
048000980320     C                   SETOFF                                       464748
048100130515     C                   SETOFF                                       49  28
048200110913      *  Note ESPLICATIVE
048300110913     C     *INKB         IFEQ      '1'
048400110913     c                   exsr      winNOTE
048500110913     C                   GOTO      FINVD2
048600110913     C                   END
048700110913     C*
048800100210      * Forza sigla e non emette vusalizzazione di errore
048900100210     c     *INKH         IFEQ      '1'
049000100210     c                   move      'S'           Forza_sigla       1
049100100210     C                   END
049200960607     C*  Fine Lavoro
049300960607     C     *INKC         IFEQ      '1'
049400960607     C                   MOVEL     'S'           WFINE
049500960611     C                   GOTO      FINVD2
049600960607     C                   END
049700960607     C*  Ritorno
049800960607     C     *INKL         IFEQ      '1'
049900130509     C                   MOVEL     '1'           $GESD
050000960607     C                   GOTO      FINVD2
050100960607     C                   END
050200960607     C*  Annullamento
050300960607     C     *INKQ         IFEQ      '1'
050400960607     C                   EXSR      ANNUL
050500960607     C                   GOTO      FINVD2
050600960607     C                   END
050700960607     C*  Controlli
050800960607     C                   EXSR      CTR02
050900960607     C   28              GOTO      FINVD2
051000960607     C*  Ripristino
051100960607     C     *INKE         IFEQ      '1'
051200960607     C                   EXSR      RIPRIS
051300960607     C                   GOTO      FINVD2
051400960607     C                   END
051500070216     C*
051600960607     C*  Conferma
051700960607     C     *INKF         IFEQ      '1'
051800960607     C                   EXSR      CONFER
051900960627     C                   GOTO      FINVD2
052000960607     C                   END
052100960627     C*  Invio: Terza videata
052200070216     C                   EXSR      INZ03
052300960607     C*
052400960607     C     FINVD2        ENDSR
052500130515     C*---------------------------------------------------------------*
052600130515     C*  CTR02: Controlli seconda videata
052700130515     C*---------------------------------------------------------------*
052800130515     C     CTR02         BEGSR
052900130515     C*
053000130515     1* Ricerca alfabeitica codice partner
053100130515     C     V2CKSC        IFEQ      0
053200130515     C     V2DKSC        ANDNE     *BLANKS
053300130515     C                   MOVEL     RSUT          PARDUT           30
053400130515     C                   MOVEL     V2DKSC        ACORAG
053500130515     C                   Z-ADD     1             PARKUT            1 0
053600130515     C                   Z-ADD     KCI           ACOKCC
053700130515     C                   CALL      'XALFA1'
053800130515     C                   PARM                    PARDUT
053900130515     C                   PARM                    PARKUT
054000130515     C                   PARM                    ACORAG
054100130515     C                   PARM                    ACOKCC
054200130515     C                   PARM                    ACOKSC
054300130515     C                   PARM                    PARSTA            1 0
054400130515     C                   PARM                    PARFLR           90
054500130515     C*  Controllo se aggiornare decodifica e codice
054600130515     C     PARSTA        IFNE      -1
054700130515     C                   MOVEL     ACORAG        V2DKSC
054800130515     C                   Z-ADD     ACOKSC        V2CKSC
054900130515     C                   END
055000130515     C                   END
055100130515     C*  Codice partner
055200130515     C     V2CKSC        IFEQ      0
055300130515     C                   SETON                                        4028
055400130515     C                   MOVEL     ERR(1)        $MSG                           cod.partner
055500130515     C                   GOTO      FINCT2                                       obbligatorio
055600130515     C                   END
055700130515     C*  Codice partner di cliente inesistente
055800130515     C                   Z-ADD     KCI           KKCC
055900130515     C                   Z-ADD     V2CKSC        KKSC
056000130515     C     KACO          CHAIN     CNACO00F
056100130515     C                   IF        not %Found(CNACO00F)
056200130515     C                   SETON                                        4028
056300130515     C                   MOVEL     ERR(2)        $MSG
056400130515     C                   GOTO      FINCT2
056500130515     C                   ELSE
056600130515     C                   MOVEL     ACORAG        V2DKSC
056700130515     C                   END
056800130515      *
056900130515      * Se cambiato codice deve ricordare di andare a vedere la tabella 3K
057000130515      *  e modificare il membro su cui scrivere i dati del TIVGD
057100130515     C     V2CKSC        IFne      H2cKSC
057200130515     C                   Exfmt     TC93W02
057300130515     C                   END
057400130515     C*  Annullato
057500130515     C     ACOFLG        IFEQ      '*'
057600130515     C                   SETON                                        4028
057700130515     C                   MOVEL     ERR(3)        $MSG
057800130515     C                   GOTO      FINCT2
057900130515     C                   END
058000130515      *
058100130515     C*  Codice partner di cliente inesistente
058200130515     c                   if        V2Cexp > 0  and
058300130515     c                             V2CTPC = 'P'
058400130515     C                   Z-ADD     KCI           KKCC
058500130515     C                   Z-ADD     V2Cexp        KKSC
058600130515     C     KACO          CHAIN     CNACO00F
058700130515     C                   IF        not %Found(CNACO00F)
058800130515     C                   SETON                                        5028
058900130515     C                   MOVEL     ERR(2)        $MSG
059000130515     C                   GOTO      FINCT2
059100130515     C                   ELSE
059200130515     C                   MOVEL     ACORAG        V2Dexp
059300130515     C                   END
059400130515     C                   Else
059500130515     C                   clear                   V2Cexp
059600130515     C                   clear                   V2Dexp
059700130515     C                   END
059800130520      ****
059900130520      * NAZIONE
060000130520     C*  Ricerca Nazione
060100130520     C     '?'           SCAN      V2CNAZ                                 32
060200130520     C     *IN32         IFEQ      '1'
060300130520     C                   Z-ADD     1             §KUT
060400130520     C                   MOVEL     '15'          §COD
060500130520     C                   CALL      'X§TABER'
060600130520     C                   PARM                    §KUT
060700130520     C                   PARM                    §COD
060800130520     C                   PARM                    §KEY
060900130520     C                   PARM                    §DES             25
061000130520     C                   MOVEL     §KEY          V2CNAZ
061100130520     C                   END
061200130520     C*  Nazione obbligatoria
061300131010     C***  V2CNAZ        IFEQ      *BLANKS
061400131010     C*********          SETON                                        4328
061500131010     C********           MOVEL     ERR(7)        $MSG
061600131010     C*******            GOTO      FINCT2
061700131010     C******             END
061800130520     C*  Decodifico nazione
061900130520     C                   Z-ADD     1             X
062000130520     C     V2CNAZ        LOOKUP    C15(X)                                 30
062100130520     C     *IN30         IFEQ      '0'
062200130520     C                   SETON                                        4328
062300130520     C                   MOVEL     ERR(8)        $MSG
062400130520     C                   GOTO      FINCT2
062500130520     C                   ELSE
062600130520     C                   MOVEL     D15(X)        V2DNAZ
062700130520     C                   END
062800130520      ****
062900130520      *  LINEA PARTENZA
063000130520     C*  Controllo lnp
063100130520     C     V2CLNP        IFEQ      0
063200130520     C                   SETON                                        4128
063300130520     C                   MOVEL     ERR(4)        $MSG                           manca lnp
063400130520     C                   GOTO      FINCT2
063500130520     C                   END
063600130520     C*  lnp inesistente
063700130520     C                   Z-ADD     V2CLNP        ORGFIL
063800130520     C     ORGFIL        CHAIN     AZORG01L
063900130520     C                   IF        not %Found(AZORG01L)
064000130520     C                   SETON                                        4128
064100130520     C                   MOVEL     ERR(5)        $MSG
064200130520     C                   GOTO      FINCT2
064300130520     C                   ELSE
064400130520     C                   MOVEL     ORGDES        V2DLNP
064500130520     C                   END
064600130520     C*  Annullato
064700130520     C     ORGFVA        IFEQ      '*'
064800130520     C                   SETON                                        4128
064900130520     C                   MOVEL     ERR(6)        $MSG
065000130520     C                   GOTO      FINCT2
065100130520     C                   END
065200160205     C*  Controllo fgs
065300160205     c                   clear                   v2dfgs
065400160205     C     V2fgs         IFgt      0
065500160209     C     V2fgs         IFeq      V2CLNP
065600160209     C                   SETON                                        5128
065700160209     C                   MOVEL     ERR(35)       $MSG
065800160209     C                   GOTO      FINCT2
065900160209     c                   else
066000160205     C*  lnp inesistente
066100160205     C                   Z-ADD     V2fgs         ORGFIL
066200160205     C     ORGFIL        CHAIN     AZORG01L
066300160205     C                   IF        not %Found(AZORG01L)
066400160205     C                   SETON                                        5128
066500160205     C                   MOVEL     ERR(34)       $MSG
066600160205     C                   GOTO      FINCT2
066700160205     C                   ELSE
066800160205     C                   MOVEL     ORGDES        V2Dfgs
066900160205     C                   END
067000160205     C*  Annullato
067100160205     C     ORGFVA        IFEQ      '*'
067200160205     C                   SETON                                        5128
067300160205     C                   MOVEL     ERR(35)       $MSG
067400160205     C                   GOTO      FINCT2
067500160205     C                   END
067600160205     C                   END
067700160209     c                   endIF
067800130520      ****
067900130520      *  TARIFFA
068000130520     C*  Codice Tariffa obbligatorio
068100130520     C     V2CCTR        IFEQ      *BLANKS
068200130520     C                   SETON                                        4528
068300130520     C                   MOVEL     ERR(13)       $MSG
068400130520     C                   GOTO      FINCT2
068500130520     C                   END
068600130520      ****
068700130520      * COD TRATT.MERCE
068800130520     C*  Ricerca Codice trattamento merce
068900130520     C     '?'           SCAN      V2CCTM                                 32
069000130520     C     *IN32         IFEQ      '1'
069100130520     C                   Z-ADD     1             §KUT
069200130520     C                   MOVEL     '1B'          §COD
069300130520     C                   CALL      'X§TABER'
069400130520     C                   PARM                    §KUT
069500130520     C                   PARM                    §COD
069600130520     C                   PARM                    §KEY
069700130520     C                   PARM                    §DES
069800130520     C                   MOVEL     §KEY          V2CCTM
069900130520     C                   END
070000130520     C*  Codice trattamento merce
070100130520     C     V2CCTM        IFEQ      *BLANKS
070200130520     C                   SETON                                        4428
070300130520     C                   MOVEL     ERR(10)       $MSG
070400130520     C                   GOTO      FINCT2
070500130520     C                   END
070600130520     C*  Controllo se codice trattamento merce esiste
070700130520     C                   Z-ADD     1             X
070800130520     C     V2CCTM        LOOKUP    C1B(X)                                 30
070900130520     C     *IN30         IFEQ      '0'
071000130520     C                   SETON                                        4428
071100130520     C                   MOVEL     ERR(11)       $MSG
071200130520     C                   GOTO      FINCT2
071300130520     C                   ELSE
071400130520     C                   MOVEL     D1B(X)        DS1B
071500130520     C                   END
071600130520     C*  Se codice trattamento merce prevede che il cliente
071700130520     C*  segnacolli deve essere indicata la serie
071800130520     C     §1BFG1        IFEQ      'S'
071900130520     C     V2CNRS        ANDEQ     0
072000130520     C                   SETON                                        424428
072100130520     C                   MOVEL     ERR(12)       $MSG
072200130520     C                   GOTO      FINCT2
072300130520     C                   END
072400130520      ****
072500130520      *   LUOGO
072600130520     C*  luogo di partenza/arrivo
072700130520     C     V2CLOC        IFEQ      *BLANKS
072800130520     C                   SETON                                        4728
072900130520     C                   MOVEL     ERR(14)       $MSG
073000130520     C                   GOTO      FINCT2
073100130520     C                   END
073200130520      ****
073300130520      *   PLATFORM
073400130520     C*  Codice Platform List
073500130520     C     V2CPLT        IFEQ      *BLANKS
073600130520     C                   SETON                                        4628
073700130520     C                   MOVEL     ERR(9)        $MSG
073800130520     C                   GOTO      FINCT2
073900130520     C                   END
074000130520      ****
074100130515     C*  Controllo Sigla
074200130515     C     V2Csig        IFEQ      *blank
074300130515     C     V2Cnomfile    IFEQ      'S'
074400130515     C                   SETON                                        4928
074500130515     C                   MOVEL     ERR(28)       $MSG                           manca sigla
074600130515     c                   GOTO      FINCT2
074700130515     c                   end
074800130515     C                   Else
074900130515     c                   z-add     1             NSig
075000130515     c     V2Csig        lookup    Psig(Nsig)                             49
075100130515     c   49              do
075200130515     c                   movel     v2ckey        cod31            31
075300130515     c                   if        cod31 <> Csig(Nsig)
075400130515     c                             and Forza_sigla = ' '
075500130515     C                   SETON                                        28
075600130515     C                   MOVEL     ERR(29)       $MSG                           c'è già la sigla
075700130515     C                   GOTO      FINCT2
075800130515     c                   else
075900130515     c                   setoff                                       49
076000130515     c                   end
076100130515     c                   end
076200130515     C                   END
076300130515     C*
076400130515     C     FINCT2        ENDSR
076500130515     C*---------------------------------------------------------------*
076600130515     C*  INZ03: Inizializzo TERZA videata
076700130515     C*---------------------------------------------------------------*
076800130515     C     INZ03         BEGSR
076900130515     C*
077000130515     C                   SETOFF                                       414228
077100130515     C                   MOVEL     '3'           $GESD
077200130515     C                   MOVEL     *BLANKS       $MSG
077300130515     C                   MOVEL     NOMPGM        V3CPGM
077400130515     C                   MOVEL     V2CKEY        V3CKEY
077500130515      **
077600130515     c                   if          trovata_PT = 'S'
077700130515      **
077800130515     C                   MOVEL     §PTCMR        V3CCMR
077900130515     C                   MOVEL     §PTNFV        V3CNFV
078000130515     C                   MOVEL     §PTDES        V3CDES
078100130529      * il primo flag a sinistra del campo
078200130529     C                   MOVEL     §PTxxx        V3Cias
078300130515      **
078400130515     c                   if          trovata_PU = 'S'
078500130515      **
078600130515     C                   Z-ADD     §PUEL1        V3CEL1
078700130515     C                   Z-ADD     §PULP1        V3CLP1
078800130515     C                   Z-ADD     §PULA1        V3CLA1
078900130515     C                   Z-ADD     §PUNR1        V3CNR1
079000130515     C                   Z-ADD     §PUSG1        V3CSG1
079100130515     C                   Z-ADD     §PUZN1        V3CZN1
079200130515     C                   MOVEL     §PUWRK        V3CWRK
079300130515     C                   MOVEL     §PULNA        V3CLNA
079400130515     C                   MOVEL     §PUCAS        V3CCAS
079500130515     C                   MOVEL     §PUnsp        V3Cnsp
079600130515     C                   move      §PUlun        V3Clun
079700130515     C                   MOVEL     §PUnas        V3Cnas
079800130515     C                   MOVEL     §PUss         V3Css
079900130515     C                   MOVEL     §PUtb         V3Ctb
080000130515     C                   MOVEL     §PUtc         V3Ctc
080100130515     C                   MOVEL     §PUts         V3Cts
080200130515     C                   MOVEL     §PUgr         V3Cgr
080300130515     C                   MOVEL     §PUpfa        V3Cpfa
080400130515     C                   MOVEL     §PURAGGDES    V3ragDES
080500130515     C                   MOVEL     §PUNOMECMR    V3nomCMR
080600130515     C                   MOVEL     §PUINGIO      V3inGIO
080700130515      **
080800130515     C                   end
080900130515     C*
081000130515     c                   if          trovata_PZ = 'S'
081100130515      **
081200130515     C                   MOVEL     §PZNADSF      V3NADSF
081300130515     C*
081400130515     C                   end
081500130515     C*
081600130515     C                   endIF
081700130515      **
081800130515     C                   ENDSR
081900960627     C*---------------------------------------------------------------*
082000960627     C*  GESD03: Gestione terza videata
082100960627     C*---------------------------------------------------------------*
082200960627     C     GESD03        BEGSR
082300960627     C*
082400110321     c     v3cNSP        comp      'S'                                    35
082500111027     c     v3cWRK        comp      'S'                                    36
082600111027     c     v3ragDES      comp      'S'                                    37
082700111027     c  n37v3nomCMR      comp      *blank                             3737
082800111027     c     v3cres        comp      'S'                                    38
082900111027     C*
083000960627     C                   EXFMT     TC93D03
083100960827     C                   SETOFF                                       404142
083200100921     C                   SETOFF                                       474849
083300960911     C                   SETOFF                                       28
083400110913      *  Note ESPLICATIVE
083500110913     C     *INKB         IFEQ      '1'
083600110913     c                   exsr      winNOTE
083700110913     C                   GOTO      FINVD3
083800110913     C                   END
083900960627     C*  Fine Lavoro
084000960627     C     *INKC         IFEQ      '1'
084100960627     C                   MOVEL     'S'           WFINE
084200960627     C                   GOTO      FINVD3
084300960627     C                   END
084400960627     C*  Ritorno
084500960627     C     *INKL         IFEQ      '1'
084600130509     C                   MOVEL     '2'           $GESD
084700960627     C                   GOTO      FINVD3
084800960627     C                   END
084900960627     C*  Annullamento
085000960627     C     *INKQ         IFEQ      '1'
085100960627     C                   EXSR      ANNUL
085200960627     C                   GOTO      FINVD3
085300960627     C                   END
085400960627     C*  Controlli
085500960627     C                   EXSR      CTR03
085600960627     C   28              GOTO      FINVD3
085700960627     C*  Ripristino
085800960627     C     *INKE         IFEQ      '1'
085900960627     C                   EXSR      RIPRIS
086000960827     C                   GOTO      FINVD3
086100960627     C                   END
086200960627     C*  Conferma
086300960627     C     *INKF         IFEQ      '1'
086400960827     C                   EXSR      CONFER
086500960827     C                   GOTO      FINVD3
086600960627     C                   END
086700960911     C*  Invio: Sesta videata
086800960911     C                   EXSR      INZ06
086900960627     C*
087000960627     C     FINVD3        ENDSR
087100130515     C*---------------------------------------------------------------*
087200130515     C*  CTR03: Controlli terza videata
087300130515     C*---------------------------------------------------------------*
087400130515     C     CTR03         BEGSR
087500130515     C*
087600130515     C     V3Cwrk        IFNE      'S'
087700130515     C     V3ragdes      IFeq      'S'
087800130515     C                   SETON                                        2847
087900130515     C                   MOVEL     ERR(32)       $MSG
088000130515     c                   GOTO      FINCT3
088100130515     c                   end
088200130515     C     V3nomcmr      IFne      *blank
088300130515     C                   SETON                                        2848
088400130515     C                   MOVEL     ERR(32)       $MSG
088500130515     c                   GOTO      FINCT3
088600130515     c                   end
088700130515     C     V3ingio       IFeq      'S'
088800130515     C                   SETON                                        2849
088900130515     C                   MOVEL     ERR(32)       $MSG
089000130515     c                   GOTO      FINCT3
089100130515     c                   end
089200130515     c                   end
089300130515      *
089400130515     C     V3Cwrk        IFEQ      'S'
089500130515     C     V3ragdes      ANDEQ     'S'
089600130515     C     V3nomcmr      ANDEQ     *blank
089700130515     C                   SETON                                        2848
089800130515     C                   MOVEL     ERR(33)       $MSG
089900130515     c                   GOTO      FINCT3
090000130515     c                   end
090100130515     C*
090200130515     C*  Se non mi passano segnacollo bartolini non devono essere
090300130515     C*  immessi nr.elementi-posizione iniz.lnp/lna/nrs/sgn/znc
090400130515     C     V2CBS1        IFNE      'S'
090500130515      *
090600130515     C     V3CEL1        IFGT      0
090700130515      * ....e solo se non è un Disk C
090800130515     c**********   Tolto il controllo poichè YSL ci ha costretto a fare
090900130515     c**********   un altro ragionamento su segnacolli EEX e non solo
091000130515     c**********   Bartolini.
091100130515     C***  §1bf12        IFeq      'N'
091200130515     C***  §1bDKC        andne     'C'
091300130515     C*******            SETON                                        2841
091400130515     C*******            MOVEL     ERR(20)       $MSG
091500130515     C*******            GOTO      FINCT3
091600130515     c**********         end
091700130515     C                   END
091800130515      *
091900130515     C     V3CLP1        IFGT      0
092000130515     C     V3CLA1        ORGT      0
092100130515     C     V3CNR1        ORGT      0
092200130515     C     V3CSG1        ORGT      0
092300130515     C     V3CZN1        ORGT      0
092400130515     C                   SETON                                        2840
092500130515     C                   MOVEL     ERR(20)       $MSG
092600130515     C                   GOTO      FINCT3
092700130515     C                   END
092800130515     C*
092900130515     C                   ELSE
093000130515      * questo campo ha assunto una funzione limitativa ossia se il partner
093100130515      * invia + codici e vogliamo prendere solo alcuni allora occorre indicare
093200130515      * il numero di elementi validi da prendere e questi devono essere
093300130515      * ovviamente dal primo in avanti.
093400130515      * Così se indico 3 ed il partner manda 5 segnacolli prenderemo solo i
093500130515      * primi 3 ignorando il 4 ed il 5. (YSL ci ha costretto ad utilizzare
093600130515      * tale metodo) il campo quindi vale sia x segnacolli EEX che Bartolini adesso.
093700130515     C**** V3CEL1        IFEQ      0
093800130515     C*******            SETON                                        284041
093900130515     C*******            MOVEL     ERR(21)       $MSG
094000130515     C*******            GOTO      FINCT3
094100130515     C*******            END
094200130515     C     V3CLP1        IFEQ      0
094300130515     C     V3CLA1        ANDEQ     0
094400130515     C     V3CNR1        ANDEQ     0
094500130515     C     V3CSG1        ANDEQ     0
094600130515     C     V3CZN1        ANDEQ     0
094700130515     C                   SETON                                        284042
094800130515     C                   MOVEL     ERR(21)       $MSG
094900130515     C                   GOTO      FINCT3
095000130515     C                   END
095100130515     C*
095200130515     C                   END
095300130515     C*
095400130515     C     FINCT3        ENDSR
095500130515     C*---------------------------------------------------------------*
095600130515     C*  INZ04: Inizializzo QUARTA videata
095700130515     C*---------------------------------------------------------------*
095800130515     C     INZ04         BEGSR
095900130515     C*
096000130515     C                   MOVEL     '4'           $GESD
096100130515     C                   MOVEL     *BLANKS       $MSG
096200130515     C                   MOVEL     NOMPGM        V4CPGM
096300130515     C                   MOVEL     V2CKEY        V4CKEY
096400130515     C*
096500130515     C*  La PT è quella che deve esserci
096600130515     c                   if          trovata_PT = 'S'
096700130515     C*
096800130515     C                   MOVEL     §PTFTT        V4CFTT
096900130515     C                   MOVEL     §PTCM2        V4CCM2
097000130515     C                   MOVEL     §PTNF2        V4CNF2
097100130515     C                   MOVEL     §PTLC1        V4CLO1
097200130515     C                   MOVEL     §PTLC2        V4CLO2
097300130515     C                   MOVEL     §PTDT1        V4CDT1
097400130515     C                   MOVEL     §PTEQD        V4CEQD
097500130515     C***                  MOVEL§PTTRD    V4CTRD
097600130515     C                   MOVEL     §PTCN1        V4CCN1
097700130515     C                   MOVEL     §PTCN2        V4CCN2
097800130515     C                   MOVEL     §PTCN3        V4CCN3
097900130515     C                   MOVEL     §PTCN4        V4CCN4
098000130515     C*********          MOVEL     §PTNSP        V4CNSP
098100130515     C                   MOVEL     §PTAGE        V4CAGE
098200130515      *
098300130515     c                   if          trovata_PU = 'S'
098400130515     C                   MOVEL     §PUSPC        V4CSPC
098500130515     C                   MOVEL     §PUNSA        V4CNSA
098600130515     C                   MOVEL     §PUSEC        V4CSEC
098700130515     C                   MOVEL     §PUVOL        V4CVOL
098800130515     C                   MOVEL     §PUVma        V4Vman
098900130515     C                   MOVEL     §PUISC        V4cISC
099000130515     C                   MOVEL     §PUftxAAA     V4ftxAAA
099100130515     c                   end
099200130515      *
099300130515     c                   if          trovata_PV = 'S'
099400130515     C                   MOVEL     §PVCURESO     V4CURESO
099500130515     C                   MOVEL     §PVCUQUAR     V4CUQUR
099600130515     C                   MOVEL     §PVCUTIPR     V4CUTIPR
099700130515     C                   MOVEL     §PVCUORM      V4CUORM
099800130515     C                   MOVEL     §PVCUQUAO     V4CUQUO
099900130515     C                   MOVEL     §PVCUTIPO     V4CUTIPO
100000130515     C                   MOVEL     §PVquaPCI     V4quaPCI
100100130515     C                   MOVEL     §PVpciXseg    V4xSEG
100200130515      * riempie con (0)
100300130515     c                   if          v4xSEG = *blank
100400130515     c                   eval           v4xSEG = *zeros
100500130515     c                   end
100600130515      *----
100700130515      * inizializza il campo numerico se non presente in tabella
100800130515     C                   MOVEL     §PUGGV        V4CGGV
100900130515     c                   end
101000130515      *
101100130515     c                   if          trovata_PZ = 'S'
101200130515     C                   MOVEL     §PZtsrGWC     V4CGWC
101300130515     C                   MOVEL     §PZsuffTAB    V4sufTB
101400130515     c                   end
101500170412     C*
101600170412     C                   clear                   V4sALERT
101700170412     c                   if          trovata_PS = 'S'
101800170412     C                   MOVEL     §PSsALERT     V4sALERT
101900170412     c                   end
102000130515     C*
102100130515     C                   ELSE
102200130515     C*
102300130515     C                   MOVEL     'W'           V4CFTT
102400130515     C                   MOVEL     'N'           V4CCM2
102500130515     C                   MOVEL     'N'           V4CNF2
102600130515     C                   MOVEL     'N'           V4CLO1
102700130515     C                   MOVEL     'N'           V4CLO2
102800130515     C                   MOVEL     'N'           V4CDT1
102900130515     C                   MOVEL     'N'           V4CEQD
103000130515     C                   MOVEL     'N'           V4CCN1
103100130515     C                   MOVEL     'N'           V4CCN2
103200130515     C                   MOVEL     'N'           V4CCN3
103300130515     C                   MOVEL     'N'           V4CCN4
103400130515     C                   MOVEL     'N'           V4CSPC
103500130515     C*******            Z-ADD     0             V4CNSP
103600130515     C                   Z-ADD     0             V4CAGE
103700130520     C                   MOVEL     'S'           V4CNSA
103800130515     C                   MOVEL     'N'           V4CSEC
103900130515     C                   MOVEL     'N'           V4CVOL
104000130515     C                   MOVEL     'N'           V4CURESO
104100130515     C                   MOVEL     'N'           V4CUORM
104200130515     C                   Z-ADD     1             V4CGGV
104300130515     C                   clear                   V4sufTB
104400130520     C                   MOVEL     'N'           V4CGWC
104500130520     C                   MOVEL     'N'           V4ftxAAA
104600130520     C                   MOVEL     'N'           V4Vman
104700130520     c                   eval           v4xSEG = '01'
104800130522     C                   MOVEL     ' '           V4cISC
104900130520     C                   MOVEL     '14'          V4CAGE
105000170412     C                   move      'N'           V4sALERT
105100130515     C                   END
105200130515     C*
105300130515     C                   ENDSR
105400960827     C*---------------------------------------------------------------*
105500960827     C*  GESD04: Gestione quarta videata
105600960827     C*---------------------------------------------------------------*
105700960827     C     GESD04        BEGSR
105800130215     C*  metter in rosso - x evidenziare  la presenza del codice suffisso
105900130215     c     v4sufTB       comp      *blank                             5959
106000130215     C*
106100960827     C                   EXFMT     TC93D04
106200110913      *  Note ESPLICATIVE
106300110913     C     *INKB         IFEQ      '1'
106400110913     c                   exsr      winNOTE
106500110913     C                   GOTO      FINVD4
106600110913     C                   END
106700960827     C*  Fine Lavoro
106800960827     C     *INKC         IFEQ      '1'
106900960827     C                   MOVEL     'S'           WFINE
107000960827     C                   GOTO      FINVD4
107100960827     C                   END
107200960827     C*  Ritorno
107300960827     C     *INKL         IFEQ      '1'
107400130509     C                   MOVEL     '7'           $GESD
107500960827     C                   GOTO      FINVD4
107600960827     C                   END
107700960827     C*  Annullamento
107800960827     C     *INKQ         IFEQ      '1'
107900960827     C                   EXSR      ANNUL
108000960827     C                   GOTO      FINVD4
108100960827     C                   END
108200960827     C*  Ripristino
108300960827     C     *INKE         IFEQ      '1'
108400960827     C                   EXSR      RIPRIS
108500960827     C                   GOTO      FINVD4
108600960827     C                   END
108700960827     C*  Conferma
108800960827     C     *INKF         IFEQ      '1'
108900960827     C                   EXSR      CONFER
109000960827     C                   GOTO      FINVD4
109100960827     C                   END
109200130515     C*  Invio: alla videata 08
109300070131     C                   EXSR      INZ08
109400960827     C*
109500960827     C     FINVD4        ENDSR
109600130515     C*---------------------------------------------------------------*
109700130515     C*  INZ05: Inizializzo Quinta videata
109800130515     C*---------------------------------------------------------------*
109900130515     C     INZ05         BEGSR
110000130515     C*
110100130515     C                   SETOFF                                       4028
110200130515     C                   MOVEL     '5'           $GESD
110300130515     C                   MOVEL     *BLANKS       $MSG
110400130515     C                   MOVEL     NOMPGM        V5CPGM
110500130515     C                   MOVEL     V2CKEY        V5CKEY
110600130515     C                   MOVEL     TABFTT        V5CFTT
110700130515     C                   MOVEL     TABFLT        V5CFLT
110800130515     C*
110900130515     C                   ENDSR
111000960827     C*---------------------------------------------------------------*
111100960827     C*  GESD05: Gestione quinta videata
111200960827     C*---------------------------------------------------------------*
111300960827     C     GESD05        BEGSR
111400960827     C*
111500070216     C*  se il partner gestisce + linee x + nazioni
111600070216     c     v2Cpln        comp      'S'                                    08
111700070216     C*  per ora non lo abilito (08 = *Off)
111800070216     c                   setoff                                       08
111900140221     C*
112000140221     C* neutralizzo le 2 specifiche precedenti sopra perchè:
112100140221     C*  come Utility:
112200140221     C*     ABILITO FORZATAMENTE la possibilità di DUPLICA
112300140221     c                   setON                                        08
112400140221     C*
112500960827     C                   EXFMT     TC93D05
112600960827     C                   SETOFF                                       4028
112700110913      *  Note ESPLICATIVE
112800110913     C     *INKB         IFEQ      '1'
112900110913     c                   exsr      winNOTE
113000110913     C                   GOTO      FINVD5
113100110913     C                   END
113200960827     C*  Fine Lavoro
113300960827     C     *INKC         IFEQ      '1'
113400960827     C                   MOVEL     'S'           WFINE
113500960827     C                   GOTO      FINVD5
113600960827     C                   END
113700960827     C*  Ritorno
113800960827     C     *INKL         IFEQ      '1'
113900130509     C                   MOVEL     'P'           $GESD
114000960827     C                   GOTO      FINVD5
114100960827     C                   END
114200960827     C*  Annullamento
114300960827     C     *INKQ         IFEQ      '1'
114400960827     C                   EXSR      ANNUL
114500960827     C                   GOTO      FINVD5
114600960827     C                   END
114700960827     C*  Controlli
114800960827     C                   EXSR      CTR05
114900960827     C   28              GOTO      FINVD5
115000960827     C*  Ripristino
115100960827     C     *INKE         IFEQ      '1'
115200960827     C                   EXSR      RIPRIS
115300960827     C                   GOTO      FINVD5
115400960827     C                   END
115500070216     C* se abilitato per duplicare l'UNB
115600070216     C     *inKH         ifEQ      *on
115700070216     C     *in08         andeq     *on
115800070216     c     aggiorna_db   andeq     'S'
115900070216     c                   exsr      Wind_Duplica
116000070216     C                   GOTO      FINVD5
116100070216     C                   END
116200960827     C*  Conferma
116300960827     C     *INKF         IFEQ      '1'
116400960827     C                   EXSR      CONFER
116500960827     C                   END
116600960827     C*
116700960827     C     FINVD5        ENDSR
116800130515     C*---------------------------------------------------------------*
116900130515     C*  CTR05: Controlli quinta videata
117000130515     C*---------------------------------------------------------------*
117100130515     C     CTR05         BEGSR
117200130515     C*
117300130515     C*  Controllo se filale da indicare
117400130515     C     V5CFTT        IFEQ      '2'
117500130515     C*  Trasmissione solo a  una filiale
117600130515     C     V5CFLT        IFEQ      0
117700130515     C                   SETON                                        4028
117800130515     C                   MOVEL     ERR(15)       $MSG
117900130515     C                   GOTO      FINCT5
118000130515     C                   ELSE
118100130515     C                   Z-ADD     V5CFLT        ORGFIL
118200130515     C     ORGFIL        CHAIN     AZORG01L
118300130515     C                   IF        not %Found(azorg01l)
118400130515     C                   SETON                                        4028
118500130515     C                   MOVEL     ERR(16)       $MSG
118600130515     C                   GOTO      FINCT5
118700130515     C                   END
118800130515     C                   END
118900130515     C*  Trasmissione a tutti o a nessuno
119000130515     C                   ELSE
119100130515     C     V5CFLT        IFNE      0
119200130515     C                   SETON                                        4028
119300130515     C                   MOVEL     ERR(17)       $MSG
119400130515     C                   GOTO      FINCT5
119500130515     C                   END
119600130515     C                   END
119700130515     C*
119800130515     C     FINCT5        ENDSR
119900130515     C*---------------------------------------------------------------*
120000130515     C*  Duplica UNB x Partner con + linee x + nazioni                *
120100130515     C*---------------------------------------------------------------*
120200130515     C     Wind_Duplica  Begsr
120300130515     C*
120400130515     C* Salva la chiave da cui generare la duplica
120500130515     C                   MOVEL     V2CKEY        savKey_UNB       35
120600130515     C*
120700130515     c                   clear                   tc93w01
120800130515     C                   MOVEL     V2CKEY        w1unb1
120900130515     C*
121000130515     c     window_01     tag
121100130515     c                   exfmt     tc93w01
121200130515     c                   setoff                                       99
121300130515     C* Annulla Duplica
121400130515     C     *INKC         IFEQ      '1'
121500130515     C                   MOVEL     'S'           WFINE
121600130515     c                   end
121700130515     c   Kc
121800130515     cor Kl              goto      end_w01
121900130515     C*
122000130515     C*    controlli campi chiave
122100130515     C                   exsr      contr_W01
122200130515     c  nKf
122300130515     cor 99              goto      window_01
122400130515     C*
122500130515     C* Esegue  Duplica
122600130515     c   Kf              exsr      duplica_UNB
122700130515     C*
122800130515     C     end_w01       EndSR
122900130515     C*---------------------------------------------------------------*
123000130515     C*  contr_W01 -  Controlli sui campi  principali                 *
123100130515     C*---------------------------------------------------------------*
123200130515     C     contr_W01     BegSR
123300130515     C*
123400130515     C* Codice nazione x nuovo UNB
123500130515     C                   MOVEL     'PT'          KCOD
123600130515     C                   MOVEL     w1unb1        codice35         35
123700130515     C                   MOVE      w1unb2        codice35
123800130515     C                   MOVEL     codice35      Kkey
123900130515     C     KTAB          chain(n)  EDTAB01L
124000130515     c                   if        %Found(EDTAB01L)
124100130515     c                   seton                                        9951
124200130515     c                   goto      err_w01
124300130515     c                   end
124400130515     c                   if        w1unb2 = *blank
124500130515     c                   seton                                        9953
124600130515     c                   goto      err_w01
124700130515     c                   end
124800130515     C*
124900130515     C* Linea obbligatoria
125000130515     c                   if        w1clnp = *zeros
125100130515     c                   seton                                        9955
125200130515     c                   goto      err_w01
125300130515     c                   end
125400130515     C*
125500130515     C* Solo se un Partner controllo la linea che non deve essere già
125600130515     C*  stata immessa.
125700130515     c                   if        §puTPC = 'P' and w1clnp > 0
125800130515     c     w1clnp        lookup    PTlnp                                  52
125900130515     c                   if        *in52 = *on
126000130515     c                   seton                                        9952
126100130515     c                   goto      err_w01
126200130515     c                   end
126300130515     c                   end
126400130515     C*
126500130515     C* Nazione
126600130515     c                   if        w1cnaz = *blank
126700130515     c                   seton                                        9957
126800130515     c                   goto      err_w01
126900130515     c                   end
127000130515     C*
127100130515     C     err_w01       EndSR
127200130515     C*---------------------------------------------------------------*
127300130515     C*  duplica   -  le tabelle                                      *
127400130515     C*---------------------------------------------------------------*
127500130515     C     duplica_UNB   BegSR
127600130515     C*
127700130515     C*  Gestisco scrittura/aggiornamento key
127800130515     C*
127900130515     C                   MOVEL     *BLANKS       KKEY
128000130515     C                   MOVEL     V2CKEY        KKEY
128100130515     C                   MOVEL     'PT'          KCOD
128200130515     C     KTAB          CHAIN     EDTAB01L
128300130515     c                   if        %Found(EDTAB01L)
128400130515     C                   MOVEL     TABUNI        DSPT
128500130515      *
128600130515      * imposta i campi di scrittura PT
128700130515     c                   clear                   edtab000
128800130515     c                   if        w1cksc > 0
128900130515     c                   movel     w1cksc        §ptKSC
129000130515     c                   end
129100130515     c                   movel     w1clnp        §ptLNP
129200130515     c                   movel     w1cNAZ        §ptNAZ
129300130515     C                   MOVEL     DSPT          TABUNI
129400130515     C                   MOVEL     'PT'          TABCOD
129500130515     C                   MOVEL     codice35      TABKEY
129600130515     c                   write     edtab000
129700130515     c                   end
129800130515     C*
129900130515     C                   MOVEL     'PU'          KCOD
130000130515     C     KTAB          CHAIN     EDTAB01L
130100130515     c                   if        %Found(EDTAB01L)
130200130515     C                   MOVEL     TABUNI        DSPU
130300140221     c                   end
130400130515      *
130500130515      * imposta i campi di scrittura PU
130600130515     c                   clear                   edtab000
130700130515     C                   MOVEL     DSPU          TABUNI
130800130515     C                   MOVEL     'PU'          TABCOD
130900130515     C                   MOVEL     codice35      TABKEY
131000130515     c                   write     edtab000
131100140221     c*********          end
131200130515     C*
131300130515     C                   MOVEL     'PV'          KCOD
131400130515     C     KTAB          CHAIN     EDTAB01L
131500130515     c                   if        %Found(EDTAB01L)
131600130515     C                   MOVEL     TABUNI        DSPV
131700140221     c                   end
131800130515      *
131900130515      * imposta i campi di scrittura PV
132000130515     c                   clear                   edtab000
132100130515     C                   MOVEL     DSPV          TABUNI
132200130515     C                   MOVEL     'PV'          TABCOD
132300130515     C                   MOVEL     codice35      TABKEY
132400130515     c                   write     edtab000
132500140221     c*********          end
132600130515     C*
132700130515     C                   MOVEL     'PW'          KCOD
132800130515     C     KTAB          CHAIN     EDTAB01L
132900130515     c                   if        %Found(EDTAB01L)
133000130515     C                   MOVEL     TABUNI        DSPW
133100140221     c                   end
133200130515      *
133300130515      * imposta i campi di scrittura PW
133400130515     c                   clear                   edtab000
133500130515     C                   MOVEL     DSPW          TABUNI
133600130515     C                   MOVEL     'PW'          TABCOD
133700130515     C                   MOVEL     codice35      TABKEY
133800130515     c                   write     edtab000
133900140221     c*********          end
134000130515     C*
134100130515     C                   MOVEL     'PZ'          KCOD
134200130515     C     KTAB          CHAIN     EDTAB01L
134300130515     c                   if        %Found(EDTAB01L)
134400130515     C                   MOVEL     TABUNI        DSPZ
134500140221     c                   end
134600130515     C*
134700130515      * imposta i campi di scrittura PZ
134800130515     c                   clear                   edtab000
134900130515     C                   MOVEL     DSPZ          TABUNI
135000130515     C                   MOVEL     'PZ'          TABCOD
135100130515     C                   MOVEL     codice35      TABKEY
135200130515     c                   write     edtab000
135300140221     c*********          end
135400140221     C*
135500140221     C                   MOVEL     'PS'          KCOD
135600140221     C     KTAB          CHAIN     EDTAB01L
135700140221     c                   if        %Found(EDTAB01L)
135800140221     C                   MOVEL     TABUNI        DSPS
135900140221     c                   end
136000140221     C*
136100140221      * imposta i campi di scrittura PS
136200140221     c                   clear                   edtab000
136300140221     C                   MOVEL     DSPS          TABUNI
136400140221     C                   MOVEL     'PS'          TABCOD
136500140221     C                   MOVEL     codice35      TABKEY
136600140221     c                   write     edtab000
136700140221     c*********          end
136800130515     C*
136900130515     C                   MOVEL     'PO'          KCOD
137000130515     C     KTAB          CHAIN     EDTAB01L
137100130515     c                   if        %Found(EDTAB01L)
137200130515     C                   MOVEL     TABUNI        DSPO
137300140221     c                   end
137400130515     C*
137500130515      * imposta i campi di scrittura PO
137600130515     c                   clear                   edtab000
137700130515     C                   MOVEL     DSPO          TABUNI
137800130515     C                   MOVEL     'PO'          TABCOD
137900130515     C                   MOVEL     codice35      TABKEY
138000130515     c                   write     edtab000
138100140221     c*********          end
138200130515     C*
138300130515     C*  NON RIPORTA LE NOTE in DUPLICA
138400130515     C* Mostra la videata con il nuovo UNB da modificare in altri campi
138500130515     C                   MOVEL     codice35      v1ckey
138600130515     C                   MOVEL     '1'           $GESD             1
138700130515     C*
138800130515     C                   EndSR
138900130515     C*---------------------------------------------------------------*
139000130515     C*  INZ06: Inizializzo SESTA videata
139100130515     C*---------------------------------------------------------------*
139200130515     C     INZ06         BEGSR
139300130515     C*
139400130515     C                   SETOFF                                       404142
139500130515     C                   SETOFF                                       434428
139600130515     C                   SETOFF                                       49
139700130515     C                   MOVEL     '6'           $GESD
139800130515     C                   MOVEL     *BLANKS       $MSG
139900130515     C                   MOVEL     NOMPGM        V6CPGM
140000130515     C                   MOVEL     V2CKEY        V6CKEY
140100150112     C                   clear                   V6CAPP
140200150112     C                   clear                   V6Crcu
140300160706     C                   MOVEL     'S'           V6ftxnad
140400161013     C                   clear                   V6nadCN
140500161013     C                   clear                   V6cnsu21
140600130515      *
140700130515     c                   if          trovata_PT = 'S'
140800130515     C                   MOVEL     §PTCM1        V6CCM1
140900130515     C                   MOVEL     §PTNF1        V6CNF1
141000130515     C                   MOVEL     §PTDET        V6CDET
141100130515     C                   MOVEl     §PTTRD        V6CTRD
141200130515     C                   MOVEl     §PTSGN        V6CRMN
141300130515      ****
141400130515     c                   if          trovata_PU = 'S'
141500130515     C                   Z-ADD     §PUEL2        V6CEL2
141600130515     C                   Z-ADD     §PULP2        V6CLP2
141700130515     C                   Z-ADD     §PULA2        V6CLA2
141800130515     C                   Z-ADD     §PUNR2        V6CNR2
141900130515     C                   Z-ADD     §PUSG2        V6CSG2
142000130515     C                   Z-ADD     §PUZN2        V6CZN2
142100130515     C                   MOVEL     §PULAB        V6CLAB
142200130515     C                   MOVEL     §PURFF        V6CRFF
142300130515     C                   MOVEL     §PUdate203    V6C203
142400130515     C                   MOVEL     §PUFIRMA      V6CFIRMA
142500130515     c                   end
142600140221      ****
142700140221     c                   if          trovata_PS = 'S'
142800140221     C                   MOVEL     §PSAPP        V6CAPP
142900140416     C                   MOVEL     §PSrcu        V6Crcu
143000160706     C                   MOVEL     §PSftxnad     V6ftxnad
143100161013     C                   MOVEL     §PSNADCN      V6nadCN
143200161013     C                   MOVEL     §PScnsu21     V6cnsu21
143300140221     c                   end
143400130515      ****
143500130515     C                   Else
143600130515     C*
143700130515     C                   Z-ADD     1             V6CEL2
143800130515     C                   END
143900130515     C*
144000130515     C                   ENDSR
144100960911     C*---------------------------------------------------------------*
144200960911     C*  GESD06: Gestione sesta videata
144300960911     C*---------------------------------------------------------------*
144400960911     C     GESD06        BEGSR
144500960911     C*
144600111207     C*  in reverse se si Inviano STATUS diversi dai nostri standard
144700111207     c                   setoff                                       51
144800111207     c                   if        v6CLAB ='S'
144900111207     c                   seton                                        51
145000111207     c                   end
145100111207     C*
145200960911     C                   EXFMT     TC93D06
145300960911     C                   SETOFF                                       404142
145400961011     C                   SETOFF                                       434445
145500961021     C                   SETOFF                                       434428
145600050831     C                   SETOFF                                       49
145700110913      *  Note ESPLICATIVE
145800110913     C     *INKB         IFEQ      '1'
145900110913     c                   exsr      winNOTE
146000110913     C                   GOTO      FINVD6
146100110913     C                   END
146200960911     C*  Fine Lavoro
146300960911     C     *INKC         IFEQ      '1'
146400960911     C                   MOVEL     'S'           WFINE
146500960911     C                   GOTO      FINVD6
146600960911     C                   END
146700960911     C*  Ritorno
146800960911     C     *INKL         IFEQ      '1'
146900130509     C                   MOVEL     '3'           $GESD
147000960911     C                   GOTO      FINVD6
147100960911     C                   END
147200960911     C*  Annullamento
147300960911     C     *INKQ         IFEQ      '1'
147400960911     C                   EXSR      ANNUL
147500960911     C                   GOTO      FINVD6
147600960911     C                   END
147700960911     C*  Controlli
147800960911     C                   EXSR      CTR06
147900960911     C   28              GOTO      FINVD6
148000960911     C*  Ripristino
148100960911     C     *INKE         IFEQ      '1'
148200960911     C                   EXSR      RIPRIS
148300960911     C                   GOTO      FINVD6
148400960911     C                   END
148500960911     C*  Conferma
148600960911     C     *INKF         IFEQ      '1'
148700960911     C                   EXSR      CONFER
148800960911     C                   GOTO      FINVD6
148900960911     C                   END
149000961021     C*  Invio: SETTIMA videata
149100961021     C                   EXSR      INZ07
149200960911     C*
149300960911     C     FINVD6        ENDSR
149400130515     C*---------------------------------------------------------------*
149500130515     C*  CTR06: Controlli sesta videata
149600130515     C*---------------------------------------------------------------*
149700130515     C     CTR06         BEGSR
149800130515     C*
149900130515     C*  Se numero CMR da ritornare obbligatorio indicarlo
150000130515     C     V3CCMR        IFEQ      ' '
150100130515     C     V6CCM1        ANDEQ     'S'
150200130515     C                   SETON                                        4028
150300130515     C                   MOVEL     ERR(18)       $MSG
150400130515     C                   GOTO      FINCT6
150500130515     C                   END
150600130515     C*  Se numero foglio viaggio da ritornare obbligatorio indicarlo
150700130515     C     V3CNFV        IFEQ      ' '
150800130515     C     V6CNF1        ANDEQ     'S'
150900130515     C                   SETON                                        4128
151000130515     C                   MOVEL     ERR(19)       $MSG
151100130515     C                   GOTO      FINCT6
151200130515     C                   END
151300130515     C*  Se numero sgn. bartolini da ritornare obbligatorio indicarlo
151400130515     C     V2CBS1        IFEQ      ' '
151500130515     C     V6CDET        ANDNE     'M'
151600130515     C                   SETON                                        4228
151700130515     C                   MOVEL     ERR(22)       $MSG
151800130515     C                   GOTO      FINCT6
151900130515     C                   END
152000130515      ***
152100130515     C     V6CDET        IFne      'M'
152200130515     C     V6CEL2        ifEQ      0
152300130515     C     V6CEL2        orGT      10
152400130515     C                   SETON                                        2845
152500130515     C                   MOVEL     ERR(30)       $MSG
152600130515     C                   GOTO      FINCT6
152700130515     c                   end
152800130515     c                   end
152900130515      ***
153000130515     C*  Se non devo ritornare sgn. bartolini non devono essere
153100130515     C*  immessi nr.elementi-posizione iniz.lnp/lna/nrs/sgn/znc
153200140416     C*
153300130515     C     V6CDET        IFEQ      'M'
153400140416     C*
153500130515     C     V6CEL2        IFGT      0
153600130515     C                   SETON                                        284245
153700130515     C                   MOVEL     ERR(23)       $MSG
153800130515     C                   GOTO      FINCT6
153900130515     C                   END
154000130515      ***
154100130515     C     V6CLP2        IFGT      0
154200130515     C     V6CLA2        ORGT      0
154300130515     C     V6CNR2        ORGT      0
154400130515     C     V6CSG2        ORGT      0
154500130515     C     V6CZN2        ORGT      0
154600130515     C                   SETON                                        2840
154700130515     C                   MOVEL     ERR(23)       $MSG
154800130515     C                   GOTO      FINCT6
154900130515     C                   END
155000130515     C*
155100130515     C                   ELSE
155200140416     C*
155300130515     C     V2CBS1        IFEQ      'S'
155400140416     C*
155500130515     C     V6CEL2        IFEQ      0
155600140416     C                   SETON                                        284045
155700130515     C                   MOVEL     ERR(24)       $MSG
155800130515     C                   GOTO      FINCT6
155900130515     C                   END
156000140416      *
156100140416      *  prima erano in "AND" trasformate in "OR" perchè devono esserci tutte
156200140416      *   le definizioni del NOSTRO segnacollo
156300130515     C     V6CLP2        IFEQ      0
156400140416     C     V6CLA2        orEQ      0
156500140416     C     V6CNR2        orEQ      0
156600140416     C     V6CSG2        orEQ      0
156700140416     C     V6CZN2        orEQ      0
156800130515     C                   SETON                                        284042
156900130515     C                   MOVEL     ERR(24)       $MSG
157000130515     C                   GOTO      FINCT6
157100130515     C                   END
157200130515     C*
157300130515     C                   END
157400130515     C                   END
157500130515     C*
157600130515     C     FINCT6        ENDSR
157700130515     C*---------------------------------------------------------------*
157800130515     C*  INZ07: Inizializzo SETTIMA VIdeata
157900130515     C*---------------------------------------------------------------*
158000130515     C     INZ07         BEGSR
158100130515     C*
158200130515     C                   SETOFF                                       404142
158300130515     C                   SETOFF                                       4328
158400130515     C                   MOVEL     '7'           $GESD
158500130515     C                   MOVEL     *BLANKS       $MSG
158600130515     C                   MOVEL     NOMPGM        V7CPGM
158700130515     C                   MOVEL     V2CKEY        V7CKEY
158800130515      ****
158900130515     c                   if          trovata_PT = 'S'
159000130515      ****
159100130515     c                   if          trovata_PU = 'S'
159200130515     C                   MOVEL     §PUDTA        V7CDTA
159300130515     C                   MOVEL     §PUECC        V7CECC
159400130515     C                   MOVEL     §PUDTB        V7CDTB
159500130515     C                   MOVEL     §PUSGD        V7CSGD
159600130515     C                   MOVEL     §PUATR        V7CATR
159700130515     c                   end
159800130515      *
159900130515     c                   if          trovata_PV = 'S'
160000130515     C                   MOVEL     §PVCNVSTS     V7CCNVSTS
160100130515     C                   MOVEL     §PVIDENT      V7CIDEnt
160200130515     c                   end
160300130515      *
160400130515     C                   ELSE
160500130515     C                   MOVEL     'M'           V7CSGD
160600130515     C                   END
160700130515     C*
160800130515     C                   ENDSR
160900961021     C*---------------------------------------------------------------*
161000961021     C*  GESD07: Gestione settima videata
161100961021     C*---------------------------------------------------------------*
161200961021     C     GESD07        BEGSR
161300961021     C*
161400111221     c     V7CCNVSTS     comp      'S'                                    77
161500961021     C                   EXFMT     TC93D07
161600961021     C                   SETOFF                                       404142
161700961021     C                   SETOFF                                       4328
161800110913      *  Note ESPLICATIVE
161900110913     C     *INKB         IFEQ      '1'
162000110913     c                   exsr      winNOTE
162100110913     C                   GOTO      FINVD7
162200110913     C                   END
162300961021     C*  Fine Lavoro
162400961021     C     *INKC         IFEQ      '1'
162500961021     C                   MOVEL     'S'           WFINE
162600961021     C                   GOTO      FINVD7
162700961021     C                   END
162800961021     C*  Ritorno
162900961021     C     *INKL         IFEQ      '1'
163000130509     C                   MOVEL     '6'           $GESD
163100961021     C                   GOTO      FINVD7
163200961021     C                   END
163300961021     C*  Annullamento
163400961021     C     *INKQ         IFEQ      '1'
163500961021     C                   EXSR      ANNUL
163600961021     C                   GOTO      FINVD7
163700961021     C                   END
163800961021     C*  Controlli
163900961021     C                   EXSR      CTR07
164000961021     C   28              GOTO      FINVD7
164100961021     C*  Ripristino
164200961021     C     *INKE         IFEQ      '1'
164300961021     C                   EXSR      RIPRIS
164400961021     C                   GOTO      FINVD7
164500961021     C                   END
164600961021     C*  Conferma
164700961021     C     *INKF         IFEQ      '1'
164800961021     C                   EXSR      CONFER
164900961021     C                   GOTO      FINVD7
165000961021     C                   END
165100111221     C*
165200111221     C*  Prima di passare Oltre se è attivata la funzione
165300111221     C*   di ricezione IFTSTA con codici diversi dal nostro Standard
165400111221     C*    apre il programma per poterli codificare sul file EDSTS00F
165500111221     C*     in modo da avere tutto correlato.
165600111221     c     V7CCNVSTS     ifeq      'S'
165700111221     C                   EXSR      OPEN_SR1
165800111221     c                   end
165900111221     C*
166000961021     C*  Invio: QUARTA videata
166100961021     C                   EXSR      INZ04
166200961021     C*
166300961021     C     FINVD7        ENDSR
166400130515     C*---------------------------------------------------------------*
166500130515     C*  CTR07: Controlli settima videata
166600130515     C*---------------------------------------------------------------*
166700130515     C     CTR07         BEGSR
166800130515     C*
166900130515     C*  Se non trasmetto date arrivo no eccedenze
167000130515     C     V7CDTA        IFEQ      *BLANKS
167100130515     C     V7CECC        ANDNE     *BLANKS
167200130515     C                   SETON                                        284041
167300130515     C                   MOVEL     ERR(25)       $MSG
167400130515     C                   GOTO      FINCT7
167500130515     C                   END
167600130515     C*  Se non trasmetto date arrivo no bolle positive
167700130515     C     V7CDTA        IFEQ      *BLANKS
167800130515     C     V7CDTB        ANDNE     *BLANKS
167900130515     C                   SETON                                        284042
168000130515     C                   MOVEL     ERR(26)       $MSG
168100130515     C                   GOTO      FINCT7
168200130515     C                   END
168300130515     C*  Se non trasmetto date arrivo no sgn
168400130515     C     V7CDTA        IFEQ      *BLANKS
168500130515     C     V7CSGD        ANDNE     'M'
168600130515     C                   SETON                                        284043
168700130515     C                   MOVEL     ERR(27)       $MSG
168800130515     C                   GOTO      FINCT7
168900130515     C                   END
169000130515     C*
169100130515     C     FINCT7        ENDSR
169200130515     C*---------------------------------------------------------------*
169300130515     C*  INZ08: Inizializzo OTTAVA  VIdeata
169400130515     C*---------------------------------------------------------------*
169500130515     C     INZ08         BEGSR
169600130515     C*
169700130515     C                   SETOFF                                       414243
169800130515     C                   SETOFF                                       28
169900130515     C                   MOVEL     '8'           $GESD
170000130515     C                   MOVEL     *BLANKS       $MSG
170100130515     C                   MOVEL     NOMPGM        V8CPGM
170200130515     C                   MOVEL     V2CKEY        V8CKEY
170300130515     C*
170400130515     c                   if          trovata_PT = 'S'
170500130515     C*
170600130515     c                   if          trovata_PV = 'S'
170700130515     C                   MOVEL     §PVRFF        V8CRFF
170800130515     C                   MOVEL     §PVRFS        V8CRFS
170900130515     C                   MOVEL     §PVRSE        V8CRSE
171000130515     C                   MOVEL     §PVRCU        V8CRCU
171100130515     C                   MOVEL     §PVpes        V8Cpes
171200130515     C                   MOVEL     §PVcol        V8Ccol
171300130515     C                   MOVEL     §PVsin        V8Csin
171400130515     C                   MOVEL     §PVicn        V8Cicn
171500130515     C                   MOVEL     §PV2in        V8C2in
171600130515     C                   MOVEL     §PVinv        V8Cinv
171700130515     C                   MOVEL     §PVcniRMN     V8Ccni
171800130515     C                   END
171900130515     C*
172000130515     C                   END
172100130515     C*
172200130515     C                   ENDSR
172300070131     C*---------------------------------------------------------------*
172400070131     C*  GESD08: Gestione ottava videata
172500070131     C*---------------------------------------------------------------*
172600070131     C     GESD08        BEGSR
172700070131     C*
172800070131     C                   EXFMT     TC93D08
172900070131     C                   SETOFF                                       28
173000110913      *  Note ESPLICATIVE
173100110913     C     *INKB         IFEQ      '1'
173200110913     c                   exsr      winNOTE
173300110913     C                   GOTO      FINVD8
173400110913     C                   END
173500070131     C*  Fine Lavoro
173600070131     C     *INKC         IFEQ      '1'
173700070131     C                   MOVEL     'S'           WFINE
173800070131     C                   GOTO      FINVD8
173900070131     C                   END
174000070131     C*  Ritorno
174100070131     C     *INKL         IFEQ      '1'
174200130509     C                   MOVEL     '4'           $GESD
174300070131     C                   GOTO      FINVD8
174400070131     C                   END
174500070131     C*  Annullamento
174600070131     C     *INKQ         IFEQ      '1'
174700070131     C                   EXSR      ANNUL
174800070131     C                   GOTO      FINVD8
174900070131     C                   END
175000070131     C*  Controlli
175100070131     C                   EXSR      CTR08
175200070131     C   28              GOTO      FINVD8
175300070131     C*  Ripristino
175400070131     C     *INKE         IFEQ      '1'
175500070131     C                   EXSR      RIPRIS
175600070131     C                   GOTO      FINVD8
175700070131     C                   END
175800070131     C*  Conferma
175900070131     C     *INKF         IFEQ      '1'
176000070131     C                   EXSR      CONFER
176100070131     C                   GOTO      FINVD8
176200070131     C                   END
176300101019     C*  Invio: videata di riepilogo
176400101019     C                   EXSR      INZriep
176500070131     C*
176600070131     C     FINVD8        ENDSR
176700130515     C*---------------------------------------------------------------*
176800130515     C*  CTR08: Controlli ottava videata
176900130515     C*---------------------------------------------------------------*
177000130515     C     CTR08         BEGSR
177100130515     C*
177200130515     C                   SETOff                                       4128
177300130515     c                   clear                   ult_char          3 0
177400130515     c                   eval      ult_char = %len(%trimr(V8EMAI))
177500130515     C*
177600130515     C*  gli indirizzi immessi devono finire con (;) separatore
177700130515     c                   if        ult_char > 0
177800130515     C                   IF        %subst(v8emai:ult_char:1) <> ';'
177900130515     C                   SETON                                        4128
178000130515     C                   MOVEL     ERR(31)       $MSG
178100130515     C                   GOTO      FINCT8
178200130515     C                   END
178300130515     c                   end
178400130515     C*
178500130515     C     FINCT8        ENDSR
178600130515     C*---------------------------------------------------------------*
178700130515     C*  INZriep Inizializzo VIdeata dati riepilogativi
178800130515     C*---------------------------------------------------------------*
178900130515     C     INZriep       BEGSR
179000130515     C*
179100130515     C                   SETOFF                                       414243
179200130515     C                   SETOFF                                       2866
179300130515     C*
179400130515     C                   MOVEL     'P'           $GESD
179500130515     C                   MOVEL     NOMPGM        VPTNPGM
179600130515     C                   MOVEL     *BLANKS       $MSG
179700130515     C*
179800130515     C                   MOVEL     v2ckey        VPTNUNB
179900130515     C                   MOVEL     V2cKSC        VPTNcli
180000130515     C                   clear                   VPTNCOM
180100130515     C                   clear                   VPTNMAIL
180200130515     c                   z-add     0             VPT_3KKSU
180300130522     c                   clear                   VPOsndPRTF
180400130515     c                   clear                   v2trsm
180500130515     C*
180600130515     C                   clear                   VRCZFB
180700130515     C                   clear                   VRCZTB
180800130515     C                   clear                   VRCZPB
180900130515     C                   clear                   VTRSFB
181000130515     C                   clear                   VTRSPB
181100130515     C                   clear                   VTRSTB
181200130515     C                   clear                   VRCZFS
181300130515     C                   clear                   VRCZPS
181400130515     C                   clear                   VRCZTS
181500130515     C                   clear                   VTRSFS
181600130515     C                   clear                   VTRSPS
181700130515     C                   clear                   VTRSTS
181800130515     C                   clear                   VRCZFO
181900130515     C                   clear                   VRCZPO
182000130515     C                   clear                   VRCZTO
182100130515     C                   clear                   VTRSFO
182200130515     C                   clear                   VTRSPO
182300130515     C                   clear                   VTRSTO
182400130515      *---
182500130515     C                   clear                   ORCZFB
182600130515     C                   clear                   ORCZPB
182700130515     C                   clear                   ORCZTB
182800130515     C                   clear                   OTRSFB
182900130515     C                   clear                   OTRSPB
183000130515     C                   clear                   OTRSTB
183100130515     C                   clear                   ORCZFS
183200130515     C                   clear                   ORCZPS
183300130515     C                   clear                   ORCZTS
183400130515     C                   clear                   OTRSFS
183500130515     C                   clear                   OTRSPS
183600130515     C                   clear                   OTRSTS
183700130515     C                   clear                   ORCZFO
183800130515     C                   clear                   ORCZPO
183900130515     C                   clear                   ORCZTO
184000130515     C                   clear                   OTRSFO
184100130515     C                   clear                   OTRSPO
184200130515     C                   clear                   OTRSTO
184300130515      *
184400130515     c                   if        v3cwrk = 'S'
184500130515     C                   MOVEL     'SI'          VPTNCMR
184600130515     c                   else
184700130515     C                   MOVEL     'NO'          VPTNCMR
184800130515     c                   end
184900130515     C*
185000130515     C*  Controlla subito se strutturalmente vi sono delle parti del msg EDI
185100130515     C*  DEFINITE diversamente dallo standard
185200130515     C*  se ci sono dei campi da tradurre in modo diverso devono essere definiti
185300130515     C*  nel file tabella EDI EDSTBL00F ..... quindi imposta una dicitura a video
185400130515     C*  a pro-memoria accendendo l'ind.66
185500130515     C     V2cKEY        setll     edstbl01l
185600130515     c                   if        %equal
185700130515     c                   seton                                        66
185800130515     c                   end
185900130515     C*
186000130515     C                   move      '1'           tblkut
186100130515     C                   move      '3K'          tblcod
186200130515     C                   movel(p)  V2cKSC        tblkey
186300130515     C     Ktabel        chain     tabel00f
186400130515     c                   if        %Found(tabel00f)
186500130515     c                   eval      ds3k = tblUNI
186600130515     c                   z-add     §3KCKS        VPT_3KKSU
186700130515     c                   end
186800130515     C*
186900130515     c                   if          trovata_PT = 'S'
187000130515     C*
187100130515     c                   if          trovata_PU = 'S'
187200130515     C                   MOVEL     §puXXX        VPTxxx
187300130515     c                   if        §puXXX = 'S'
187400130515     c                   eval      v2trsm = 'ON'
187500130515     c                   end
187600130515     c                   end
187700130515      *
187800130515     c                   if         trovata_PW  = 'S'
187900130515     C                   MOVEL     §PWCOMMER     VPTNCOM
188000130515     C                   MOVEL     §PWCONTACT    VPTNMAIL
188100130515     c                   end
188200130515      *
188300130515     c                   if         trovata_PZ  = 'S'
188400130515      *
188500130515     C                   MOVEL     §PZTIPRCZB    VRCZTB
188600130515     C                   MOVEL     §PZFILRCZB    VRCZFB
188700130515     C                   MOVEL     §PZPGMRCZB    VRCZPB
188800130515     C                   MOVEL     §PZTIPTRSB    VTRSTB
188900130515     C                   MOVEL     §PZFILTRSB    VTRSFB
189000130515     C                   MOVEL     §PZPGMTRSB    VTRSPB
189100130515     C                   MOVEL     §PZTIPRCZS    VRCZTS
189200130515     C                   MOVEL     §PZFILRCZS    VRCZFS
189300130515     C                   MOVEL     §PZPGMRCZS    VRCZPS
189400130515     C                   MOVEL     §PZTIPTRSS    VTRSTS
189500130515     C                   MOVEL     §PZFILTRSS    VTRSFS
189600130515     C                   MOVEL     §PZPGMTRSS    VTRSPS
189700130515     C                   MOVEL     §PZFTPCOD     VPTCONNEX
189800130515      *---
189900130515     c                   end
190000130515      *
190100130515     c                   if         trovata_PO  = 'S'
190200130515      *
190300130515     C                   MOVEL     §PoTIPRCZo    VRCZTO
190400130515     C                   MOVEL     §PoFILRCZo    VRCZFO
190500130515     C                   MOVEL     §PoPGMRCZo    VRCZPO
190600130515     C                   MOVEL     §PoTIPTRSo    VTRSTO
190700130515     C                   MOVEL     §PoFILTRSo    VTRSFO
190800130515     C                   MOVEL     §PoPGMTRSo    VTRSPO
190900130522     C                   MOVEL     §PoSNDprtf    VPOsndPRTF
191000130515      *---
191100130515     c                   end
191200130515      *---
191300130515     c                   if         trovata_PZ  = 'S' or
191400130515     c                              trovata_PO  = 'S'
191500130515     c                   exsr      ctrvRIE
191600130515     c                   end
191700130515      *---
191800130515     c                   end
191900130515      *
192000130515     C                   ENDSR
192100101001     C*---------------------------------------------------------------*
192200101019     C*  GESriep: Gestione Dati di riepilogo
192300101001     C*---------------------------------------------------------------*
192400101019     C     GESriep       BEGSR
192500101001     C*
192600101001     C                   EXFMT     TC93PTN
192700101001     C                   SETOFF                                       28
192800110913      *  Note ESPLICATIVE
192900110913     C     *INKB         IFEQ      '1'
193000110913     c                   exsr      winNOTE
193100110913     C                   GOTO      FINVrie
193200110913     C                   END
193300101001     C*  Fine Lavoro
193400101001     C     *INKC         IFEQ      '1'
193500101001     C                   MOVEL     'S'           WFINE
193600101019     C                   GOTO      FINVrie
193700101001     C                   END
193800101001     C*  Ritorno
193900101001     C     *INKL         IFEQ      '1'
194000130509     C                   MOVEL     '8'           $GESD
194100101019     C                   GOTO      FINVrie
194200101001     C                   END
194300101001     C*  Annullamento
194400101001     C     *INKQ         IFEQ      '1'
194500101001     C                   EXSR      ANNUL
194600101019     C                   GOTO      FINVrie
194700101001     C                   END
194800101001     C*  Controlli
194900101019     C                   EXSR      CTRVrie
195000101019     C   28              GOTO      FINVrie
195100101001     C*  Ripristino
195200101001     C     *INKE         IFEQ      '1'
195300101001     C                   EXSR      RIPRIS
195400101019     C                   GOTO      FINVrie
195500101001     C                   END
195600101001     C*  Conferma
195700101001     C     *INKF         IFEQ      '1'
195800101001     C                   EXSR      CONFER
195900101019     C                   GOTO      FINVrie
196000101001     C                   END
196100101001     C*  Invio: Quinta  videata
196200101001     C                   EXSR      INZ05
196300130515     C*   Abilita l'F6
196400130515     C                   SETON                                        06
196500101001     C*
196600101019     C     FINVrie       ENDSR
196700101001     C*---------------------------------------------------------------*
196800101019     C*  CTRVrie Controlli videata di file riepilogativo
196900101001     C*---------------------------------------------------------------*
197000101019     C     CTRVrie       BEGSR
197100101001     C*
197200130412     C                   SETOff                                       28
197300101020     C                   SETOff                                       414243
197400101020     C                   SETOff                                       444546
197500101020     C                   SETOff                                       474849
197600101020     C                   SETOff                                       505152
197700130412     C                   SETOff                                       535455
197800130412     C                   SETOff                                       565758
197900130522     C                   SETOff                                       5960
198000101020     c                   clear                   oRCZFB
198100130412     c                   clear                   oRCZPB
198200130412     c                   clear                   oRCZtB
198300130412      *
198400101020     c                   clear                   oTRSFB
198500110608     c                   clear                   oTRSPB
198600130412     c                   clear                   oTRStB
198700130412      *
198800130412     c                   clear                   oRCZFS
198900130412     c                   clear                   oRCZPS
199000130412     c                   clear                   oRCZtS
199100130412      *
199200130412     c                   clear                   oTRSFS
199300130412     c                   clear                   oTRSPS
199400110608     c                   clear                   oTRStS
199500130412      *
199600130412     c                   clear                   oRCZFo
199700130412     c                   clear                   oRCZPo
199800130412     c                   clear                   oRCZto
199900130412      *
200000130412     c                   clear                   oTRSFo
200100130412     c                   clear                   oTRSPo
200200130412     c                   clear                   oTRSto
200300120416      **
200400120416      **  Codice di trasmissione vuoto e richiesta la trasmissione con questo codice
200500120416     c                   if        VPTCONNEX = 0 and VPTXXX ='S'
200600120416     c                   seton                                        53  28
200700120416     c                   goto      FINCTptn
200800120416     c                   end
200900120416      **
201000101020     C* file Bolle
201100101020     c     vRCZFB        ifne      *blank
201200101020     c                   z-add     1             Npr
201300101020     c     vRCZFB        lookup    pf_key(Npr)                            21
201400101020     c                   if        not *in21
201500101020     c                   seton                                        41  28
201600101020     c                   goto      FINCTptn
201700101020     c                   else
201800101020     c                   eval      oRCZFB = pf_DS(Npr)
201900101020     c                   end
202000101020     c                   end
202100101020     C*
202200101020     c     vTRSFB        ifne      *blank
202300101020     c                   z-add     1             Npr
202400101020     c     vTRSFB        lookup    pf_key(Npr)                            21
202500101020     c                   if        not *in21
202600101020     c                   seton                                        42  28
202700101020     c                   goto      FINCTptn
202800101020     c                   else
202900101020     c                   eval      oTRSFB = pf_DS(Npr)
203000101020     c                   end
203100101020     c                   end
203200101020     C* file Stati
203300101020     c     vRCZFS        ifne      *blank
203400101020     c                   z-add     1             Npr
203500101020     c     vRCZFS        lookup    pf_key(Npr)                            21
203600101020     c                   if        not *in21
203700101020     c                   seton                                        43  28
203800101020     c                   goto      FINCTptn
203900101020     c                   else
204000101020     c                   eval      oRCZFS = pf_DS(Npr)
204100101020     c                   end
204200101020     c                   end
204300101020     C*
204400101020     c     vTRSFS        ifne      *blank
204500101020     c                   z-add     1             Npr
204600101020     c     vTRSFS        lookup    pf_key(Npr)                            21
204700101020     c                   if        not *in21
204800101020     c                   seton                                        44  28
204900101020     c                   goto      FINCTptn
205000101020     c                   else
205100101020     c                   eval      oTRSFS = pf_DS(Npr)
205200101020     c                   end
205300101020     c                   end
205400130412     C* file ORM
205500130412     c     vRCZFo        ifne      *blank
205600130412     c                   z-add     1             Npr
205700130412     c     vRCZFo        lookup    pf_key(Npr)                            21
205800130412     c                   if        not *in21
205900130412     c                   seton                                        54  28
206000130412     c                   goto      FINCTptn
206100130412     c                   else
206200130412     c                   eval      oRCZFo = pf_DS(Npr)
206300130412     c                   end
206400130412     c                   end
206500130412     C*
206600130412     c     vTRSFo        ifne      *blank
206700130412     c                   z-add     1             Npr
206800130412     c     vTRSFo        lookup    pf_key(Npr)                            21
206900130412     c                   if        not *in21
207000130412     c                   seton                                        55  28
207100130412     c                   goto      FINCTptn
207200130412     c                   else
207300130412     c                   eval      oTRSFo = pf_DS(Npr)
207400130412     c                   end
207500130412     c                   end
207600110608     C*                                 0
207700101020     C* pgm  Bolle
207800101020     c     vRCZpB        ifne      *blank
207900101020     c                   z-add     1             Npr
208000101020     c     vRCZpB        lookup    pr_key(Npr)                            21
208100101020     c                   if        not *in21
208200101020     c                   seton                                        45  28
208300101020     c                   goto      FINCTptn
208400101020     c                   else
208500110322     c                   eval      oRCZpB = pr_DS(Npr)
208600110322     c                   end
208700101020     c                   end
208800101020     C*
208900101020     c     vTRSpB        ifne      *blank
209000101020     c                   z-add     1             Npr
209100101020     c     vTRSpB        lookup    pr_key(Npr)                            21
209200101020     c                   if        not *in21
209300101020     c                   seton                                        46  28
209400101020     c                   goto      FINCTptn
209500101020     c                   else
209600110608     c                   eval      dsPR   = pr_DS(Npr)
209700110608     c                   eval      oTRSpB = §PRtraDEX
209800101020     c                   end
209900101020     c                   end
210000130412     C* pgm  Stati
210100101020     c     vRCZpS        ifne      *blank
210200101020     c                   z-add     1             Npr
210300101020     c     vRCZpS        lookup    pr_key(Npr)                            21
210400101020     c                   if        not *in21
210500101020     c                   seton                                        47  28
210600101020     c                   goto      FINCTptn
210700101020     c                   else
210800101020     c                   eval      oRCZpS = pr_DS(Npr)
210900101020     c                   end
211000101020     c                   end
211100101020     C*
211200101020     c     vTRSpS        ifne      *blank
211300101020     c                   z-add     1             Npr
211400101020     c     vTRSpS        lookup    pr_key(Npr)                            21
211500101020     c                   if        not *in21
211600101020     c                   seton                                        48  28
211700101020     c                   goto      FINCTptn
211800101020     c                   else
211900110322     c                   eval      DSpr   = pr_DS(Npr)
212000110322     c                   eval      oTRSpS = §PRTRADEX
212100101020     c                   end
212200101020     c                   end
212300130412     C* pgm  ORM
212400130412     c     vRCZpO        ifne      *blank
212500130412     c                   z-add     1             Npr
212600130412     c     vRCZpO        lookup    pr_key(Npr)                            21
212700130412     c                   if        not *in21
212800130412     c                   seton                                        58  28
212900130412     c                   goto      FINCTptn
213000130412     c                   else
213100130412     c                   eval      oRCZpO = pr_DS(Npr)
213200130412     c                   end
213300130412     c                   end
213400130412     C*
213500130412     c     vTRSpO        ifne      *blank
213600130412     c                   z-add     1             Npr
213700130412     c     vTRSpO        lookup    pr_key(Npr)                            21
213800130412     c                   if        not *in21
213900130412     c                   seton                                        59  28
214000130412     c                   goto      FINCTptn
214100130412     c                   else
214200130412     c                   eval      DSpr   = pr_DS(Npr)
214300130412     c                   eval      oTRSpO = §PRTRADEX
214400130412     c                   end
214500130412     c                   end
214600101020     C*
214700130522     C*   se Non c'è il programma di traduzione il flag di Invio LISTA di Controllo
214800130522     C*   parallela x gli ORM NON deve esserci
214900130522     c                   if        vTRSpO = *blank
215000130522     c                                       and VPOsndPRTF <> *blank
215100130522     c                   seton                                        60  28
215200130522     c                   goto      FINCTptn
215300130522     c                   end
215400130522     C*
215500101020     C* tipo invio Bolle
215600101020     c     vRCZtB        ifne      *blank
215700101020     c                   z-add     1             Npr
215800101020     c     vRCZtB        lookup    pi_key(Npr)                            21
215900101020     c                   if        not *in21
216000101020     c                   seton                                        49  28
216100101020     c                   goto      FINCTptn
216200101020     c                   else
216300101020     c                   eval      oRCZtB = pi_DS(Npr)
216400101020     c                   end
216500101020     c                   end
216600101020     C*
216700101020     c     vTRStB        ifne      *blank
216800101020     c                   z-add     1             Npr
216900101020     c     vTRStB        lookup    pi_key(Npr)                            21
217000101020     c                   if        not *in21
217100101020     c                   seton                                        50  28
217200101020     c                   goto      FINCTptn
217300101020     c                   else
217400101020     c                   eval      oTRStB = pi_DS(Npr)
217500101020     c                   end
217600101020     c                   end
217700101020     C* tipo invio Stati
217800101020     c     vRCZtS        ifne      *blank
217900101020     c                   z-add     1             Npr
218000101020     c     vRCZtS        lookup    pi_key(Npr)                            21
218100101020     c                   if        not *in21
218200101020     c                   seton                                        51  28
218300101020     c                   goto      FINCTptn
218400101020     c                   else
218500101020     c                   eval      oRCZtS = pi_DS(Npr)
218600101020     c                   end
218700101020     c                   end
218800101020     C*
218900101020     c     vTRStS        ifne      *blank
219000101020     c                   z-add     1             Npr
219100101020     c     vTRStS        lookup    pi_key(Npr)                            21
219200101020     c                   if        not *in21
219300101020     c                   seton                                        52  28
219400101020     c                   goto      FINCTptn
219500101020     c                   else
219600101020     c                   eval      oTRStS = pi_DS(Npr)
219700101020     c                   end
219800101020     c                   end
219900130412     C* tipo invio ORM
220000130412     c     vRCZtO        ifne      *blank
220100130412     c                   z-add     1             Npr
220200130412     c     vRCZtO        lookup    pi_key(Npr)                            21
220300130412     c                   if        not *in21
220400130412     c                   seton                                        56  28
220500130412     c                   goto      FINCTptn
220600130412     c                   else
220700130412     c                   eval      oRCZtO = pi_DS(Npr)
220800130412     c                   end
220900130412     c                   end
221000130412     C*
221100130412     c     vTRStO        ifne      *blank
221200130412     c                   z-add     1             Npr
221300130412     c     vTRStO        lookup    pi_key(Npr)                            21
221400130412     c                   if        not *in21
221500130412     c                   seton                                        57  28
221600130412     c                   goto      FINCTptn
221700130412     c                   else
221800130412     c                   eval      oTRStO = pi_DS(Npr)
221900130412     c                   end
222000130412     c                   end
222100101020     C*
222200101001     C*
222300101001     C     FINCTptn      ENDSR
222400960607     C*---------------------------------------------------------------*
222500960607     C*  ANNUL: Eseguo annullamento tabella                           *
222600960607     C*---------------------------------------------------------------*
222700960607     C     ANNUL         BEGSR
222800130509     C*
222900130509     c                   eval      Operazione = Annulla
223000130509     c                   exsr      AGGTabella
223100960607     C*
223200960607     C                   ENDSR
223300960607     C*---------------------------------------------------------------*
223400960607     C*  RIPRIS: Ripristino tabella annullata                         *
223500960607     C*---------------------------------------------------------------*
223600960607     C     RIPRIS        BEGSR
223700130509     C*
223800130509     c                   eval      Operazione = Ripristina
223900130509     c                   exsr      AGGTabella
224000960607     C*
224100960607     C                   ENDSR
224200130509     C*---------------------------------------------------------------*
224300130509     C*  AGGTAB: Ripristina o Annulla  la Tabella                     *
224400130509     C*---------------------------------------------------------------*
224500130509     C     AGGTABella    BEGSR
224600130509      *
224700130509     C                   MOVEL     *BLANKS       KKEY
224800130509     C                   MOVEL     V2CKEY        KKEY
224900130509     c                   z-add     0             conta_tab         3 0
225000130509      *
225100130509     c                   do        *hival
225200130509     c                   eval        conta_tab = conta_tab + 1
225300130509      *
225400130509     c                   if          conta_tab = 1
225500130509     C                   MOVEL     'PT'          KCOD
225600130509     c                   elseif      conta_tab = 2
225700130509     C                   MOVEL     'PU'          KCOD
225800130509     c                   elseif      conta_tab = 3
225900130509     C                   MOVEL     'PV'          KCOD
226000130509     c                   elseif      conta_tab = 4
226100130509     C                   MOVEL     'PW'          KCOD
226200130509     c                   elseif      conta_tab = 5
226300130509     C                   MOVEL     'PZ'          KCOD
226400130509     c                   elseif      conta_tab = 6
226500130509     C                   MOVEL     'P1'          KCOD
226600130509     c                   elseif      conta_tab = 7
226700130509     C                   MOVEL     'P2'          KCOD
226800130509     c                   elseif      conta_tab = 8
226900130509     C                   MOVEL     'P3'          KCOD
227000130509     c                   elseif      conta_tab = 9
227100130509     C                   MOVEL     'P4'          KCOD
227200130509     c                   elseif      conta_tab = 10
227300130509     C                   MOVEL     'P5'          KCOD
227400130509     c                   elseif      conta_tab = 11
227500130509     C                   MOVEL     'PO'          KCOD
227600140221     c                   elseif      conta_tab = 12
227700140221     C                   MOVEL     'PS'          KCOD
227800130509     c                   else
227900130509      *
228000130509     c                   leave
228100130509      *
228200130509     c                   end
228300130509      *
228400130509     C     KTAB          CHAIN     EDTAB01L
228500130509     C                   IF        %Found(EDTAB01L)
228600130509      **  RIPRISTINA
228700130509     c                   if        Operazione = Ripristina
228800130509     C                   MOVEL     ' '           TABFLG
228900130509     C                   MOVEL     ' '           TABFTR
229000130509     C                   Z-ADD     0             TABDTR
229100130509      **  ANNULLA
229200130509     c                   elseif    Operazione = Annulla
229300130509     C                   MOVEL     '*'           TABFLG
229400130509     C                   clear                   TABFTR
229500130509     C                   clear                   TABDTR
229600130509      **
229700130509     c                   end
229800130509      **
229900130509     C                   EXCEPT    TABANN
230000130509     C                   END
230100130509      *
230200130509     C                   ENDdo
230300130509      *
230400130509     C                   MOVEL     '1'           $GESD
230500130509     C*
230600130509     C                   ENDSR
230700130509     C*---------------------------------------------------------------*
230800960607     C*  CONFER: Confermo inserimento/variazione                      *
230900960607     C*---------------------------------------------------------------*
231000960607     C     CONFER        BEGSR
231100960607     C*
231200130509     C                   MOVEL     '1'           $GESD
231300130520     c***************    goto      endCONFER
231400110913     C*
231500960607     C*  Gestisco scrittura/aggiornamento key
231600960607     C                   MOVEL     *BLANKS       KKEY
231700960611     C                   MOVEL     V2CKEY        KKEY
231800960611     C                   MOVEL     'PT'          KCOD
231900960607     C     KTAB          CHAIN     EDTAB01L                           30
232000961021     C                   Z-ADD     V2CKSC        §PTKSC
232100960611     C                   Z-ADD     V2CLNP        §PTLNP
232200960611     C                   Z-ADD     V2CNRS        §PTNRS
232300960611     C                   MOVEL     V2CNAZ        §PTNAZ
232400960611     C                   MOVEL     V2CCTM        §PTCTM
232500960611     C                   MOVEL     V2CCTR        §PTCTR
232600960827     C                   MOVEL     V2CMSQ        §PTMSQ
232700960611     C                   MOVEL     V2CPLT        §PTPLT
232800960627     C                   MOVEL     V2CLOC        §PTLOC
232900960827     C                   MOVEL     V3CCMR        §PTCMR
233000130529      * il primo flag a sinistra del campo xxx
233100130529     C                   MOVEL     V3Cias        §PTxxx
233200960827     C                   MOVEL     V3CNFV        §PTNFV
233300960827     C                   MOVEL     V3CDES        §PTDES
233400960911     C                   MOVEL     V6CCM1        §PTCM1
233500960911     C                   MOVEL     V6CNF1        §PTNF1
233600960911     C                   MOVEL     V6CDET        §PTDET
233700960827     C                   MOVEL     V4CCM2        §PTCM2
233800960827     C                   MOVEL     V4CNF2        §PTNF2
233900960827     C                   MOVEL     V4CLO1        §PTLC1
234000960827     C                   MOVEL     V4CLO2        §PTLC2
234100960827     C                   MOVEL     V4CDT1        §PTDT1
234200960827     C                   MOVEL     V4CEQD        §PTEQD
234300960827     C                   MOVEL     V4CCN1        §PTCN1
234400960827     C                   MOVEL     V4CCN2        §PTCN2
234500960827     C                   MOVEL     V4CCN3        §PTCN3
234600960827     C                   MOVEL     V4CCN4        §PTCN4
234700960827     C                   MOVEL     V4CFTT        §PTFTT
234800120117     C********           MOVEL     V4CNSP        §PTNSP
234900960905     C                   MOVEL     V4CAGE        §PTAGE
235000100209     C                   MOVEL     V6Ctrd        §PTtrd
235100100722     C                   MOVEL     V6Crmn        §PTsgn
235200960906     C                   MOVEL     DSPT          TABUNI
235300960827     C                   MOVEL     V5CFTT        TABFTT
235400960827     C                   Z-ADD     V5CFLT        TABFLT
235500960827     C     V5CFTT        IFNE      ' '
235600960607     C                   MOVEL     ' '           TABFTR
235700960607     C                   Z-ADD     0             TABDTR
235800960607     C                   END
235900960607     C     *IN30         IFEQ      '0'
236000960607     C                   UPDATE    EDTAB000
236100960607     C                   ELSE
236200960611     C                   MOVEL     V2CKEY        TABKEY
236300960611     C                   MOVEL     'PT'          TABCOD
236400960607     C                   WRITE     EDTAB000
236500960607     C                   END
236600960905     C*  Aggiorno PU
236700960905     C                   MOVEL     'PU'          KCOD
236800960905     C     KTAB          CHAIN     EDTAB01L                           30
236900961021     C                   MOVEL     V2CTPC        §PUTPC
237000080623      * Orm Automatici: Cliente/Tariffa
237100080623     C                   move      V2Oksc        §puORMaKSC
237200080623     C                   eval      §puORMaCTR = V2Octr
237300961021     C                   MOVEL     V6CLAB        §PULAB
237400101028     C                   MOVEL     V6CFIRMA      §PUFIRMA
237500960906     C                   MOVEL     V4CNSA        §PUNSA
237600960906     C                   MOVEL     V4CSEC        §PUSEC
237700010730     C                   MOVEL     V4CVOL        §PUVOL
237800050525     C                   MOVEL     V4Vman        §PUVma
237900060912     C                   MOVEL     V4cISC        §PUisc
238000961126     C                   MOVEL     V4CSPC        §PUSPC
238100100407     C                   MOVEL     V4ftxAAA      §PUftxAAA
238200110525     C                   MOVEL     V2Compat      §PUcompat
238300980320     C                   MOVEL     V2CBS1        §PUBS1
238400050831     C                   MOVEL     V2Csig        §PUsig
238500110628     C                   MOVEL     V2Cnomfile    §PUnomfile
238600111017     C                   MOVEL     V3Cres        §PUres
238700070215     C                   MOVEL     V2Cpln        §PUpln
238800960911     C                   Z-ADD     V3CEL1        §PUEL1
238900960911     C                   Z-ADD     V3CLP1        §PULP1
239000960911     C                   Z-ADD     V3CLA1        §PULA1
239100960911     C                   Z-ADD     V3CNR1        §PUNR1
239200960911     C                   Z-ADD     V3CSG1        §PUSG1
239300960911     C                   Z-ADD     V3CZN1        §PUZN1
239400960923     C                   MOVEL     V3CWRK        §PUWRK
239500960925     C                   MOVEL     V3CLNA        §PULNA
239600120402     C                   MOVEL     VPTxxx        §PUxxx
239700100921     C                   MOVEL     V3ragDES      §PURAGGDES
239800100921     C                   MOVEL     V3nomCMR      §PUNOMECMR
239900100921     C                   MOVEL     V3inGIO       §PUINGIO
240000960926     C                   MOVEL     V3CCAS        §PUCAS
240100050223     C                   MOVEL     V3Cnsp        §PUnsp
240200050301     C                   move      V3Clun        §PUlun
240300050520     C                   move      V3Cnas        §PUnas
240400030708     C                   MOVEL     V4CGGV        §PUGGV
240500961021     C                   MOVEL     V6CRFF        §PURFF
240600960911     C                   Z-ADD     V6CEL2        §PUEL2
240700960911     C                   Z-ADD     V6CLP2        §PULP2
240800960911     C                   Z-ADD     V6CLA2        §PULA2
240900960911     C                   Z-ADD     V6CNR2        §PUNR2
241000960911     C                   Z-ADD     V6CSG2        §PUSG2
241100960911     C                   Z-ADD     V6CZN2        §PUZN2
241200100512     C                   movel     V6C203        §PUdate203
241300961021     C                   MOVEL     V7CDTA        §PUDTA
241400961021     C                   MOVEL     V7CDTB        §PUDTB
241500961021     C                   MOVEL     V7CSGD        §PUSGD
241600961021     C                   MOVEL     V7CECC        §PUECC
241700010802     C                   MOVEL     V7CATR        §PUATR
241800061127     C                   MOVEL     V3Css         §PUss
241900061127     C                   MOVEL     V3Ctb         §PUtb
242000061127     C                   MOVEL     V3Ctc         §PUtc
242100061127     C                   MOVEL     V3Cts         §PUts
242200061128     C                   MOVEL     V3Cgr         §PUgr
242300070221     C                   MOVEL     V3Cpfa        §PUpfa
242400140221      *----
242500960906     C                   MOVEL     *BLANKS       TABUNI
242600960906     C                   MOVEL     DSPU          TABUNI
242700960905     C                   MOVEL     V5CFTT        TABFTT
242800960905     C                   Z-ADD     V5CFLT        TABFLT
242900960905     C     V5CFTT        IFNE      ' '
243000960905     C                   MOVEL     ' '           TABFTR
243100960905     C                   Z-ADD     0             TABDTR
243200960905     C                   END
243300960905     C     *IN30         IFEQ      '0'
243400960905     C                   UPDATE    EDTAB000
243500960905     C                   ELSE
243600960905     C                   MOVEL     V2CKEY        TABKEY
243700960905     C                   MOVEL     'PU'          TABCOD
243800960905     C                   WRITE     EDTAB000
243900960905     C                   END
244000070123     C*  Aggiorno PV
244100070123     C                   MOVEL     'PV'          KCOD
244200070123     C     KTAB          CHAIN     EDTAB01L                           30
244300070202     C                   MOVEL     V8EMAI        §PVEMA
244400070131     C                   MOVEL     V8CRFF        §PVRFF
244500070131     C                   MOVEL     V8CRFS        §PVRFS
244600070201     C                   MOVEL     V8CRSE        §PVRSE
244700070131     C                   MOVEL     V8CRCU        §PVRCU
244800070205     C                   MOVEL     V8Cpes        §PVpes
244900070205     C                   MOVEL     V8Ccol        §PVcol
245000070206     C                   MOVEL     V8Csin        §PVsin
245100070206     C                   MOVEL     V8Cicn        §PVicn
245200070206     C                   MOVEL     V8C2in        §PV2in
245300070219     C                   MOVEL     V8Cinv        §PVinv
245400071128     C                   MOVEL     V8Ccni        §PVcniRMN
245500111221     C                   MOVEL     V7CCNVSTS     §PVCNVSTS
245600111221     C                   MOVEL     V7CIDEnt      §PVIDENT
245700120330     C                   MOVEL     V4quaPCI      §PVquaPCI
245800120330     C                   MOVEL     V4xSEG        §PVpciXseg
245900120118      *----
246000120118     C                   eval      §PVCURESO = V4CURESO
246100120118     C                   eval      §PVCUQUAR = V4CUQUR
246200120118     C                   eval      §PVCUTIPR = V4CUTIPR
246300120118     C                   eval      §PVCUORM  = V4CUORM
246400120118     C                   eval      §PVCUQUAO = V4CUQUO
246500120118     C                   eval      §PVCUTIPO = V4CUTIPO
246600120118      *----
246700070123     C                   MOVEL     *BLANKS       TABUNI
246800070123     C                   MOVEL     DSPV          TABUNI
246900070123     C                   MOVEL     V5CFTT        TABFTT
247000070123     C                   Z-ADD     V5CFLT        TABFLT
247100070123     C     V5CFTT        IFNE      ' '
247200070123     C                   MOVEL     ' '           TABFTR
247300070123     C                   Z-ADD     0             TABDTR
247400070123     C                   END
247500070123     C     *IN30         IFEQ      '0'
247600070123     C                   UPDATE    EDTAB000
247700070123     C                   ELSE
247800070123     C                   MOVEL     V2CKEY        TABKEY
247900070123     C                   MOVEL     'PV'          TABCOD
248000070123     C                   WRITE     EDTAB000
248100070123     C                   END
248200101001     C*
248300101019     C*  Aggiorno PW
248400101019     C                   MOVEL     'PW'          KCOD
248500101019     C     KTAB          CHAIN     EDTAB01L                           30
248600101019     C                   MOVEL     VPTNCOM       §PWCOMMER
248700101019     C                   MOVEL     VPTNMAIL      §PWCONTACT
248800101019     C                   MOVEL     *BLANKS       §PWNAZIONE
248900140221      *----
249000101019     C                   MOVEL     *BLANKS       TABUNI
249100101019     C                   MOVEL     DSPW          TABUNI
249200101019     C                   MOVEL     V5CFTT        TABFTT
249300101019     C                   Z-ADD     V5CFLT        TABFLT
249400101019     C     V5CFTT        IFNE      ' '
249500101019     C                   MOVEL     ' '           TABFTR
249600101019     C                   Z-ADD     0             TABDTR
249700101019     C                   END
249800101019     C     *IN30         IFEQ      '0'
249900101019     C                   UPDATE    EDTAB000
250000101019     C                   ELSE
250100101019     C                   MOVEL     V2CKEY        TABKEY
250200101019     C                   MOVEL     'PW'          TABCOD
250300101019     C                   WRITE     EDTAB000
250400101019     C                   END
250500101019     C*
250600101019     C*  Aggiorno PZ
250700101019     C                   MOVEL     'PZ'          KCOD
250800101019     C     KTAB          CHAIN     EDTAB01L                           30
250900101019     C                   MOVEL     VRCZTB        §PZTIPRCZB
251000101019     C                   MOVEL     VRCZFB        §PZFILRCZB
251100101019     C                   MOVEL     VRCZPB        §PZPGMRCZB
251200101019     C                   MOVEL     VTRSTB        §PZTIPTRSB
251300101019     C                   MOVEL     VTRSFB        §PZFILTRSB
251400101019     C                   MOVEL     VTRSPB        §PZPGMTRSB
251500101019     C                   MOVEL     VRCZTS        §PZTIPRCZS
251600101019     C                   MOVEL     VRCZFS        §PZFILRCZS
251700101019     C                   MOVEL     VRCZPS        §PZPGMRCZS
251800101019     C                   MOVEL     VTRSTS        §PZTIPTRSS
251900101019     C                   MOVEL     VTRSFS        §PZFILTRSS
252000101019     C                   MOVEL     VTRSPS        §PZPGMTRSS
252100110705     C                   MOVEL     VPTCONNEX     §PZFTPCOD
252200120206     C                   MOVEL     V2cexp        §PZkscEXP
252300120628     C                   MOVEL     V3nadSF       §PZnadSF
252400120705     C                   MOVEL     V4CGWC        §PZtsrGWC
252500130215     C                   MOVEL     V4sufTB       §PZsuffTAB
252600140221      *----
252700101019     C                   MOVEL     *BLANKS       TABUNI
252800101019     C                   MOVEL     DSPZ          TABUNI
252900101019     C                   MOVEL     V5CFTT        TABFTT
253000101019     C                   Z-ADD     V5CFLT        TABFLT
253100101019     C     V5CFTT        IFNE      ' '
253200101019     C                   MOVEL     ' '           TABFTR
253300101019     C                   Z-ADD     0             TABDTR
253400101019     C                   END
253500101019     C     *IN30         IFEQ      '0'
253600101019     C                   UPDATE    EDTAB000
253700101019     C                   ELSE
253800101019     C                   MOVEL     V2CKEY        TABKEY
253900101019     C                   MOVEL     'PZ'          TABCOD
254000101019     C                   WRITE     EDTAB000
254100101019     C                   END
254200130412     C*
254300140221     C*  Aggiorno PS
254400140221     C                   MOVEL     'PS'          KCOD
254500140221     C     KTAB          CHAIN     EDTAB01L                           30
254600140221      *
254700161013     C                   MOVEL     V6NADCN       §PSNADCN
254800161013     C                   MOVEL     V6cnsu21      §PScnsu21
254900170412     C                   MOVEL     V4sALERT      §PSsALERT
255000140221     C                   MOVEL     V6CAPP        §PSAPP
255100140416     C                   MOVEL     V6Crcu        §PSrcu
255200160706     C                   eval      §PSftxnad = V6ftxnad
255300160209     c                   if        V2fgs = 0
255400160209     C                   clear                   §PSfgs
255500160209     c                   else
255600160205     C                   MOVEL     V2fgs         §PSfgs
255700160209     c                   end
255800140221      *
255900140221     C                   MOVEL     *BLANKS       TABUNI
256000140221     C                   MOVEL     DSPS          TABUNI
256100140221     C                   MOVEL     V5CFTT        TABFTT
256200140221     C                   Z-ADD     V5CFLT        TABFLT
256300140221     C     V5CFTT        IFNE      ' '
256400140221     C                   MOVEL     ' '           TABFTR
256500140221     C                   Z-ADD     0             TABDTR
256600140221     C                   END
256700140221     C     *IN30         IFEQ      '0'
256800140221     C                   UPDATE    EDTAB000
256900140221     C                   ELSE
257000140221     C                   MOVEL     V2CKEY        TABKEY
257100140221     C                   MOVEL     'PS'          TABCOD
257200140221     C                   WRITE     EDTAB000
257300140221     C                   END
257400140221     C*
257500130412     C*  Aggiorno PO
257600130412     C                   MOVEL     'PO'          KCOD
257700130412     C     KTAB          CHAIN     EDTAB01L                           30
257800130412      *
257900130412     C                   MOVEL     VRCZTO        §POTIPRCZo
258000130412     C                   MOVEL     VRCZFO        §POFILRCZo
258100130412     C                   MOVEL     VRCZPO        §POPGMRCZo
258200130412     C                   MOVEL     VTRSTO        §POTIPTRSo
258300130412     C                   MOVEL     VTRSFO        §POFILTRSo
258400130412     C                   MOVEL     VTRSPO        §POPGMTRSo
258500130522     C                   MOVEL     VPOsndPRTF    §PoSNDprtf
258600130412      *
258700130412     C                   MOVEL     *blank        §POTIPRCZs
258800130412     C                   MOVEL     *blank        §POFILRCZs
258900130412     C                   MOVEL     *blank        §POPGMRCZs
259000130412     C                   MOVEL     *blank        §POTIPTRSs
259100130412     C                   MOVEL     *blank        §POFILTRSs
259200130412     C                   MOVEL     *blank        §POPGMTRSs
259300130412      *
259400130412     C                   MOVEL     *BLANKS       TABUNI
259500130412     C                   MOVEL     DSPO          TABUNI
259600130412     C                   MOVEL     V5CFTT        TABFTT
259700130412     C                   Z-ADD     V5CFLT        TABFLT
259800130412     C     V5CFTT        IFNE      ' '
259900130412     C                   MOVEL     ' '           TABFTR
260000130412     C                   Z-ADD     0             TABDTR
260100130412     C                   END
260200130412     C     *IN30         IFEQ      '0'
260300130412     C                   UPDATE    EDTAB000
260400130412     C                   ELSE
260500130412     C                   MOVEL     V2CKEY        TABKEY
260600130412     C                   MOVEL     'PO'          TABCOD
260700130412     C                   WRITE     EDTAB000
260800130412     C                   END
260900110819     C*
261000110913     C*  Aggiorno P1-2-3-4-5  Note (PN)
261100110913     C                   MOVEL     'P1'          KCOD
261200110913     C     KTAB          chain     EDTAB01L
261300110913      ** sia in Write che Update
261400110913     C                   MOVEL     V5CFTT        TABFTT
261500110913     C                   Z-ADD     V5CFLT        TABFLT
261600110913     C     V5CFTT        IFNE      ' '
261700110913     C                   MOVEL     ' '           TABFTR
261800110913     C                   Z-ADD     0             TABDTR
261900110913     C                   END
262000110913     c                   clear                   dsPN
262100110913     C                   eval       §Pnriga  = 1
262200110913     C                   eval       §PnNOTA  = not001
262300110913      *** non ci sono Note
262400110913     c                   if        not %Found(EDTAB01L)
262500110913     C                   MOVEL     V2CKEY        TABKEY
262600110913     C                   MOVEL     'P1'          TABCOD
262700110914     c                   z-add     data8         §PnDTINI
262800110913     c                   eval      tabUNI = dsPN
262900110913     C                   WRITE     EDTAB000
263000110913     c                   else
263100110914     c                   z-add     v2dadat       §PnDTINI
263200110913     C                   eval       tabUNI = DSPN
263300110913     C                   UPDATE    EDTAB000
263400110913     c                   end
263500110913      ***
263600110913     C                   MOVEL     'P2'          KCOD
263700110913     C     KTAB          chain     EDTAB01L
263800110913      ** sia in Write che Update
263900110913     C                   MOVEL     V5CFTT        TABFTT
264000110913     C                   Z-ADD     V5CFLT        TABFLT
264100110913     C     V5CFTT        IFNE      ' '
264200110913     C                   MOVEL     ' '           TABFTR
264300110913     C                   Z-ADD     0             TABDTR
264400110913     C                   END
264500110913     c                   clear                   dsPN
264600110913     C                   eval       §Pnriga  = 2
264700110913     C                   eval       §PnNOTA  = not002
264800110913      *** non ci sono Note
264900110913     c                   if        not %Found(EDTAB01L)
265000110913     C                   MOVEL     V2CKEY        TABKEY
265100110913     C                   MOVEL     'P2'          TABCOD
265200110914     c                   z-add     data8         §PnDTINI
265300110913     c                   eval      tabUNI = dsPN
265400110913     C                   WRITE     EDTAB000
265500110913     c                   else
265600110914     c                   z-add     v2dadat       §PnDTINI
265700110913     C                   eval       tabUNI = DSPN
265800110913     C                   UPDATE    EDTAB000
265900110913     c                   end
266000110913      ***
266100110913     C                   MOVEL     'P3'          KCOD
266200110913     C     KTAB          chain     EDTAB01L
266300110913      ** sia in Write che Update
266400110913     C                   MOVEL     V5CFTT        TABFTT
266500110913     C                   Z-ADD     V5CFLT        TABFLT
266600110913     C     V5CFTT        IFNE      ' '
266700110913     C                   MOVEL     ' '           TABFTR
266800110913     C                   Z-ADD     0             TABDTR
266900110913     C                   END
267000110913     c                   clear                   dsPN
267100110913     C                   eval       §Pnriga  = 3
267200110913     C                   eval       §PnNOTA  = not003
267300110913      *** non ci sono Note
267400110913     c                   if        not %Found(EDTAB01L)
267500110913     C                   MOVEL     V2CKEY        TABKEY
267600110913     C                   MOVEL     'P3'          TABCOD
267700110914     c                   z-add     data8         §PnDTINI
267800110913     c                   eval      tabUNI = dsPN
267900110913     C                   WRITE     EDTAB000
268000110913     c                   else
268100110914     c                   z-add     v2dadat       §PnDTINI
268200110913     C                   eval       tabUNI = DSPN
268300110913     C                   UPDATE    EDTAB000
268400110913     c                   end
268500110913      ***
268600110913     C                   MOVEL     'P4'          KCOD
268700110913     C     KTAB          chain     EDTAB01L
268800110913      ** sia in Write che Update
268900110913     C                   MOVEL     V5CFTT        TABFTT
269000110913     C                   Z-ADD     V5CFLT        TABFLT
269100110913     C     V5CFTT        IFNE      ' '
269200110913     C                   MOVEL     ' '           TABFTR
269300110913     C                   Z-ADD     0             TABDTR
269400110913     C                   END
269500110913     c                   clear                   dsPN
269600110913     C                   eval       §Pnriga  = 4
269700110913     C                   eval       §PnNOTA  = not004
269800110913      *** non ci sono Note
269900110913     c                   if        not %Found(EDTAB01L)
270000110913     C                   MOVEL     V2CKEY        TABKEY
270100110913     C                   MOVEL     'P4'          TABCOD
270200110914     c                   z-add     data8         §PnDTINI
270300110913     c                   eval      tabUNI = dsPN
270400110913     C                   WRITE     EDTAB000
270500110913     c                   else
270600110914     c                   z-add     v2dadat       §PnDTINI
270700110913     C                   eval       tabUNI = DSPN
270800110913     C                   UPDATE    EDTAB000
270900110913     c                   end
271000110913      ***
271100110913     C                   MOVEL     'P5'          KCOD
271200110913     C     KTAB          chain     EDTAB01L
271300110913      ** sia in Write che Update
271400110913     C                   MOVEL     V5CFTT        TABFTT
271500110913     C                   Z-ADD     V5CFLT        TABFLT
271600110913     C     V5CFTT        IFNE      ' '
271700110913     C                   MOVEL     ' '           TABFTR
271800110913     C                   Z-ADD     0             TABDTR
271900110913     C                   END
272000110913     c                   clear                   dsPN
272100110913     C                   eval       §Pnriga  = 5
272200110913     C                   eval       §PnNOTA  = not005
272300110913      *** non ci sono Note
272400110913     c                   if        not %Found(EDTAB01L)
272500110913     C                   MOVEL     V2CKEY        TABKEY
272600110913     C                   MOVEL     'P5'          TABCOD
272700110914     c                   z-add     data8         §PnDTINI
272800110913     c                   eval      tabUNI = dsPN
272900110913     C                   WRITE     EDTAB000
273000110913     c                   else
273100110914     c                   z-add     v2dadat       §PnDTINI
273200110913     C                   eval       tabUNI = DSPN
273300110913     C                   UPDATE    EDTAB000
273400110913     c                   end
273500110913      ***
273600110913     C*
273700110819     C*   Solo se Partner occorre allineare anche un'altra tabella x EEX
273800110819     c                   if        V2CTPC = 'P'
273900110819     c                   exsr      se_Partner
274000110819     c                   end
274100070215     C*
274200130520     C     endCONFER     ENDSR
274300130515     C*---------------------------------------------------------------*
274400130515     C*  se è un Partner EEX occorre tenere allineata la tabella
274500130515     C*  correlata con decorrenze e scadenze dei codici in vigore
274600130515     C*---------------------------------------------------------------*
274700130515     C     se_Partner    BEGSR
274800130515     C*
274900130515     C                   EXFMT     TC93W03
275000130515      **
275100130515     c                   close     TRTC93D
275200130515     C                   MOVEL     KPJBU         KPJBUs
275300130515???? c                   call      'TRTC70R'                            90
275400130515???? c                   parm                    KPJBA
275500130515     C                   MOVEL     KPJBUs        KPJBU
275600130515     c                   open      TRTC93D
275700130515     C*
275800130515     C                   endSR
275900070215     C*---------------------------------------------------------------*
276000110913     C*
276100110913     C*---------------------------------------------------------------*
276200110913     c     WinNOTE       BegSR
276300110913     C*
276400110913     C                   exfmt     tc93NOT
276500110913     c     NOT001        comp      *blank                             0202
276600110913     c  n02NOT002        comp      *blank                             0202
276700110913     c  n02NOT003        comp      *blank                             0202
276800110913     c  n02NOT004        comp      *blank                             0202
276900110913     c  n02NOT005        comp      *blank                             0202
277000110913     C*
277100110913     C                   EndSR
277200110913     C*---------------------------------------------------------------*
277300070215     C*  *INZSR: Operazioni iniziali                                  *
277400070215     C*---------------------------------------------------------------*
277500960606     C     *INZSR        BEGSR
277600960606     C*
277700960606     C     *ENTRY        PLIST
277800960606     C                   PARM                    KPJBA
277900130118     c*
278000130118     c                   open      TRTC93D
278100960606      *  Call a XPARUT
278200960606     C                   Z-ADD     1             CODUT
278300960606     C                   CALL      'X§PARUT'
278400960606     C                   PARM                    UT§DSE
278500960606     C                   MOVEL     REC80         CNCR80
278600960611     C*--- RICERCA CAPOCONTI------------------------------------------*
278700960611     C                   DO        50            X
278800960611     C                   MOVE      TCU(X)        TCUDS
278900960611     C     F56           IFEQ      'CG'
279000960611     C     F34           ANDEQ     '01'
279100960611     C                   Z-ADD     KCU(X)        KCI               4 0
279200960611     C                   END
279300960611     C                   END
279400960606     C                   MOVEL     RAGUT         RSUT
279500960606      * Definizione chiavi
279600960606     C     KTAB          KLIST
279700960606     C                   KFLD                    KCOD
279800960606     C                   KFLD                    KKEY
279900960611     C     KACO          KLIST
280000960611     C                   KFLD                    KKUT
280100960611     C                   KFLD                    KKCC
280200960611     C                   KFLD                    KKSC
280300960611     C     KTAB1         KLIST
280400960611     C                   KFLD                    KKUT
280500960611     C                   KFLD                    KCOD1
280600120416     C     Ktabel        KLIST
280700120416     C                   KFLD                    tblkut
280800120416     C                   KFLD                    tblcod
280900120416     C                   KFLD                    tblkey
281000960606      * Definizione varibili
281100960606     C     *LIKE         DEFINE    TABCOD        KCOD
281200960606     C     *LIKE         DEFINE    TABKEY        KKEY
281300960606     C     *LIKE         DEFINE    TABKEY        WINKEY
281400960611     C     *LIKE         DEFINE    ACOKUT        KKUT
281500960611     C     *LIKE         DEFINE    ACOKCC        KKCC
281600960611     C     *LIKE         DEFINE    ACOKSC        KKSC
281700960611     C     *LIKE         DEFINE    TBLCOD        KCOD1
281800960611     C     *LIKE         DEFINE    TBLKUT        §KUT
281900960611     C     *LIKE         DEFINE    TBLCOD        §COD
282000960611     C     *LIKE         DEFINE    TBLKEY        §KEY
282100960611     C     *LIKE         DEFINE    ACOKCC        PARKCC
282200960611     C     *LIKE         DEFINE    ACOKSC        PARKSC
282300960606     C* Inizializzo variabili
282400960606     C                   MOVEL     'N'           WFINE             1
282500960611     C* Caricamento dati tabella 1b
282600131010     C                   Z-ADD     0             X                 3 0
282700960611     C                   Z-ADD     1             KKUT
282800960611     C                   MOVEL     '1B'          KCOD1
282900960611     C     KTAB1         CHAIN     TABEL00F                           30
283000960611     C     *IN30         DOWEQ     '0'
283100960611     C     TBLFLG        IFEQ      *BLANKS
283200960611     C                   ADD       1             X
283300960611     C                   MOVEL     TBLKEY        C1B(X)
283400960611     C                   MOVEL     TBLUNI        D1B(X)
283500960611     C                   END
283600960611     C     KTAB1         READE     TABEL00F                               30
283700960611     C                   END
283800960611     C* Caricamento dati tabella 15
283900131010     C                   Z-ADD     0             X                 3 0
284000960611     C                   Z-ADD     1             KKUT
284100960611     C                   MOVEL     '15'          KCOD1
284200960611     C     KTAB1         CHAIN     TABEL00F                           30
284300960611     C     *IN30         DOWEQ     '0'
284400960611     C     TBLFLG        IFEQ      *BLANKS
284500960611     C                   ADD       1             X
284600960611     C                   MOVEL     TBLKEY        C15(X)
284700960611     C                   MOVEL     TBLUNI        D15(X)
284800960611     C                   END
284900960611     C     KTAB1         READE     TABEL00F                               30
285000960611     C                   END
285100960606     C*
285200051209     c                   move      'PU'          tabPU             2
285300051209     c                   z-add     0             NSig
285400051209     c     tabPU         setll     edtab01l
285500051209     c     tabPU         reade     edtab01l
285600051209     c                   dow       not %EoF(edtab01l)
285700051209     c                   movel     tabuni        dsPU
285800051209     c                   add       1             NSig
285900051209     C                   movel     §puSIG        Psig(Nsig)
286000051209     C                   movel     tabkey        Csig(Nsig)
286100051209     c     tabPU         reade     edtab01l
286200051209     c                   enddo
286300051209     C*
286400070215     C* carica tutte le linee gestite x controllo su duplica UNB
286500070215     c                   move      'PT'          tabPT             2
286600070215     c                   z-add     0             Nlnp
286700070215     c     tabPT         setll     edtab01l
286800070215     c     tabPT         reade     edtab01l
286900070215     c                   dow       not %EoF(edtab01l)
287000070215     c                   movel     tabuni        dsPT
287100070215     c                   add       1             Nlnp
287200070215     C                   z-add     §ptLNP        PTlnp(Nlnp)
287300070215     c     tabPT         reade     edtab01l
287400070215     c                   enddo
287500070215     C*
287600110608     c                   clear                   PR_DS
287700110608     c                   clear                   PR_key
287800110608     c                   clear                   PI_DS
287900110608     c                   clear                   PI_key
288000110608     c                   clear                   PF_DS
288100110608     c                   clear                   PF_key
288200101020     C*
288300101020     C* carica tutti programmi gestiti di traduzione
288400101020     c                   z-add     0             Npr               3 0
288500101020     c                   move      'PR'          tabPR             2
288600101020     c     tabPR         setll     edtab01l
288700101020     c     tabPR         reade     edtab01l
288800101020     c                   dow       not %EoF(edtab01l)
288900101020     c                   movel     tabuni        dsPR
289000101020     c                   add       1             Npr
289100101020     C                   movel     dsPR          PR_DS(Npr)
289200101020     C                   movel     TABKEY        PR_key(Npr)
289300101020     c     tabPR         reade     edtab01l
289400101020     c                   enddo
289500101020     C*
289600101020     c                   z-add     0             Npr
289700101020     c                   move      'PF'          tabPF             2
289800101020     c     tabPF         setll     edtab01l
289900101020     c     tabPF         reade     edtab01l
290000101020     c                   dow       not %EoF(edtab01l)
290100101020     c                   movel     tabuni        dsPF
290200101020     c                   add       1             Npr
290300101020     C                   movel     §PFDEFIN      PF_DS(Npr)
290400101020     C                   movel     TABKEY        PF_key(Npr)
290500101020     c     tabPF         reade     edtab01l
290600101020     c                   enddo
290700101020     C*
290800101020     c                   z-add     0             Npr
290900101020     c                   move      'PI'          tabPI             2
291000101020     c     tabPI         setll     edtab01l
291100101020     c     tabPI         reade     edtab01l
291200101020     c                   dow       not %EoF(edtab01l)
291300101020     c                   movel     tabuni        dsPI
291400101020     c                   add       1             Npr
291500101020     C                   movel     dsPI          PI_DS(Npr)
291600101020     C                   movel     TABKEY        PI_key(Npr)
291700101020     c     tabPI         reade     edtab01l
291800101020     c                   enddo
291900101020     C*
292000110914     C                   time                    datatime         14 0
292100110914     C                   move      datatime      data8             8 0
292200110914     C*
292300960606     C                   ENDSR
292400000307      *-----------------------------------------------------*
292500111221     C*
292600111221      *-----------------------------------------------------*
292700111221     C     OPEN_SR1      BEGSR
292800111221      *
292900111221     C                   MOVEL     KPJBU         KPJBUs
293000111221      *
293100111221     c                   if        v7cIDEnt = *blank
293200111221     c                   eval      XTAUNBcli = V2CKEY
293300111221     c                   else
293400111221     c                   eval      XTAUNBcli = V7Cident
293500111221     c                   end
293600111221      *
293700111221     c                   eval      XTARAGs   = v2dksc
293800111221      *
293900111221     C                   eval      KPJBU = TABDS
294000111221     c                   call      'TRTC93SR1'
294100111221     c                   parm                    KPJBA
294200111221     C                   eval      TABDS = KPJBU
294300111221      *
294400111221     C                   MOVEL     KPJBUs        KPJBU
294500111221      *
294600111221     C                   ENDSR
294700111221      *-----------------------------------------------------*
294800960607     OEDTAB000  E            TABANN
294900960607     O                       TABFLG
295000960607     O                       TABFLT
295100960607     O                       TABFTT
295200000307     O                       TABFTR
295300000307     O                       TABDTR
295400000307      *-----------------------------------------------------*
295500961011**         ERR
295600960611Codice Partner obbligatorio                                            01
295700960611Codice Partner inesistente                                             02
295800960612Codice Partner annullato                                               03
295900960611Linea di partenza obbligatoria                                         04
296000960611Linea di partenza inesistente                                          05
296100960611Linea di partenza annullata                                            06
296200960627Nazione obbligatoria                                                   07
296300960627Nazione inesistente                                                    08
296400960627Codice Platform list obbligatorio                                      09
296500960627Codice Trattamento merce obbligatorio                                  10
296600960627Codice Trattamento merce inesisitente                                  11
296700960627Codice Trattamento merce richiede il numero di serie                   12
296800960627Codice Tariffa obbligaotorio                                           13
296900960627Luogo di partenza/arrivo obbligatorio                                  14
297000960627Immettere filiale a cui trasmettere                                    15
297100960627Se trasmissione a tutti o a nessuno impossibile immettere filiale      16
297200960627Filale a cui tramettere inesistente                                    17
297300960827Se devo ritornare numero CMR dovrò riceverlo                           18
297400960827Se devo ritornare numero Foglio viaggio dovrò riceverlo                19
297500971204Se non mi passano ns.numero segnacollo non devo impostare dati sgn.    20
297600960911Se mi passano segnacollo indicare anche altri dati                     21
297700971204Se devo ritornare ns.numero segnacollo dovrò riceverlo                 22
297800140416Se NON devo inviare nostro nr.segnacollo NON impostare DATI segnac.    23
297900140416Se devo ritornare segnacollo indicare TUTTI I DATI NECESSARI           24
298000961011Impossible indicare ritorno eccedenze si se non invio le date arrivo   25
298100961011Trasmissione bolle impossibile se non previsto invio date di arrivo    26
298200961011Trasmissione segnacolli impossibile se non invio le date di arrivo     27
298300050831Sigla   obbligatoria                                                   28
298400100210Sigla già esistente su altro codice Cliente/Partner -> F8 x Forzare    29
298500051213Impostare il nr.di segnacolli da inviare nel messaggio.                30
298600070202L'ultimo carattere deve essere un (;)                                  31
298700100921é possibile solo se il VAB è fatto x la conferma  da CMR.              32
298800100921Nome CMR obbligatorio per le bolle x il raggruppamento x destinat.     33
298900160205Filiale errata inesistente                                             34
299000160209Filiale errata annullata o uguale alla LNP!                            35
