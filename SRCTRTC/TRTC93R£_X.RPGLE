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
101500130515     C*
101600130515     C                   ELSE
101700130515     C*
101800130515     C                   MOVEL     'W'           V4CFTT
101900130515     C                   MOVEL     'N'           V4CCM2
102000130515     C                   MOVEL     'N'           V4CNF2
102100130515     C                   MOVEL     'N'           V4CLO1
102200130515     C                   MOVEL     'N'           V4CLO2
102300130515     C                   MOVEL     'N'           V4CDT1
102400130515     C                   MOVEL     'N'           V4CEQD
102500130515     C                   MOVEL     'N'           V4CCN1
102600130515     C                   MOVEL     'N'           V4CCN2
102700130515     C                   MOVEL     'N'           V4CCN3
102800130515     C                   MOVEL     'N'           V4CCN4
102900130515     C                   MOVEL     'N'           V4CSPC
103000130515     C*******            Z-ADD     0             V4CNSP
103100130515     C                   Z-ADD     0             V4CAGE
103200130520     C                   MOVEL     'S'           V4CNSA
103300130515     C                   MOVEL     'N'           V4CSEC
103400130515     C                   MOVEL     'N'           V4CVOL
103500130515     C                   MOVEL     'N'           V4CURESO
103600130515     C                   MOVEL     'N'           V4CUORM
103700130515     C                   Z-ADD     1             V4CGGV
103800130515     C                   clear                   V4sufTB
103900130520     C                   MOVEL     'N'           V4CGWC
104000130520     C                   MOVEL     'N'           V4ftxAAA
104100130520     C                   MOVEL     'N'           V4Vman
104200130520     c                   eval           v4xSEG = '01'
104300130522     C                   MOVEL     ' '           V4cISC
104400130520     C                   MOVEL     '14'          V4CAGE
104500130515     C                   END
104600130515     C*
104700130515     C                   ENDSR
104800960827     C*---------------------------------------------------------------*
104900960827     C*  GESD04: Gestione quarta videata
105000960827     C*---------------------------------------------------------------*
105100960827     C     GESD04        BEGSR
105200130215     C*  metter in rosso - x evidenziare  la presenza del codice suffisso
105300130215     c     v4sufTB       comp      *blank                             5959
105400130215     C*
105500960827     C                   EXFMT     TC93D04
105600110913      *  Note ESPLICATIVE
105700110913     C     *INKB         IFEQ      '1'
105800110913     c                   exsr      winNOTE
105900110913     C                   GOTO      FINVD4
106000110913     C                   END
106100960827     C*  Fine Lavoro
106200960827     C     *INKC         IFEQ      '1'
106300960827     C                   MOVEL     'S'           WFINE
106400960827     C                   GOTO      FINVD4
106500960827     C                   END
106600960827     C*  Ritorno
106700960827     C     *INKL         IFEQ      '1'
106800130509     C                   MOVEL     '7'           $GESD
106900960827     C                   GOTO      FINVD4
107000960827     C                   END
107100960827     C*  Annullamento
107200960827     C     *INKQ         IFEQ      '1'
107300960827     C                   EXSR      ANNUL
107400960827     C                   GOTO      FINVD4
107500960827     C                   END
107600960827     C*  Ripristino
107700960827     C     *INKE         IFEQ      '1'
107800960827     C                   EXSR      RIPRIS
107900960827     C                   GOTO      FINVD4
108000960827     C                   END
108100960827     C*  Conferma
108200960827     C     *INKF         IFEQ      '1'
108300960827     C                   EXSR      CONFER
108400960827     C                   GOTO      FINVD4
108500960827     C                   END
108600130515     C*  Invio: alla videata 08
108700070131     C                   EXSR      INZ08
108800960827     C*
108900960827     C     FINVD4        ENDSR
109000130515     C*---------------------------------------------------------------*
109100130515     C*  INZ05: Inizializzo Quinta videata
109200130515     C*---------------------------------------------------------------*
109300130515     C     INZ05         BEGSR
109400130515     C*
109500130515     C                   SETOFF                                       4028
109600130515     C                   MOVEL     '5'           $GESD
109700130515     C                   MOVEL     *BLANKS       $MSG
109800130515     C                   MOVEL     NOMPGM        V5CPGM
109900130515     C                   MOVEL     V2CKEY        V5CKEY
110000130515     C                   MOVEL     TABFTT        V5CFTT
110100130515     C                   MOVEL     TABFLT        V5CFLT
110200130515     C*
110300130515     C                   ENDSR
110400960827     C*---------------------------------------------------------------*
110500960827     C*  GESD05: Gestione quinta videata
110600960827     C*---------------------------------------------------------------*
110700960827     C     GESD05        BEGSR
110800960827     C*
110900070216     C*  se il partner gestisce + linee x + nazioni
111000070216     c     v2Cpln        comp      'S'                                    08
111100070216     C*  per ora non lo abilito (08 = *Off)
111200070216     c                   setoff                                       08
111300140221     C*
111400140221     C* neutralizzo le 2 specifiche precedenti sopra perchè:
111500140221     C*  come Utility:
111600140221     C*     ABILITO FORZATAMENTE la possibilità di DUPLICA
111700140221     c                   setON                                        08
111800140221     C*
111900960827     C                   EXFMT     TC93D05
112000960827     C                   SETOFF                                       4028
112100110913      *  Note ESPLICATIVE
112200110913     C     *INKB         IFEQ      '1'
112300110913     c                   exsr      winNOTE
112400110913     C                   GOTO      FINVD5
112500110913     C                   END
112600960827     C*  Fine Lavoro
112700960827     C     *INKC         IFEQ      '1'
112800960827     C                   MOVEL     'S'           WFINE
112900960827     C                   GOTO      FINVD5
113000960827     C                   END
113100960827     C*  Ritorno
113200960827     C     *INKL         IFEQ      '1'
113300130509     C                   MOVEL     'P'           $GESD
113400960827     C                   GOTO      FINVD5
113500960827     C                   END
113600960827     C*  Annullamento
113700960827     C     *INKQ         IFEQ      '1'
113800960827     C                   EXSR      ANNUL
113900960827     C                   GOTO      FINVD5
114000960827     C                   END
114100960827     C*  Controlli
114200960827     C                   EXSR      CTR05
114300960827     C   28              GOTO      FINVD5
114400960827     C*  Ripristino
114500960827     C     *INKE         IFEQ      '1'
114600960827     C                   EXSR      RIPRIS
114700960827     C                   GOTO      FINVD5
114800960827     C                   END
114900070216     C* se abilitato per duplicare l'UNB
115000070216     C     *inKH         ifEQ      *on
115100070216     C     *in08         andeq     *on
115200070216     c     aggiorna_db   andeq     'S'
115300070216     c                   exsr      Wind_Duplica
115400070216     C                   GOTO      FINVD5
115500070216     C                   END
115600960827     C*  Conferma
115700960827     C     *INKF         IFEQ      '1'
115800960827     C                   EXSR      CONFER
115900960827     C                   END
116000960827     C*
116100960827     C     FINVD5        ENDSR
116200130515     C*---------------------------------------------------------------*
116300130515     C*  CTR05: Controlli quinta videata
116400130515     C*---------------------------------------------------------------*
116500130515     C     CTR05         BEGSR
116600130515     C*
116700130515     C*  Controllo se filale da indicare
116800130515     C     V5CFTT        IFEQ      '2'
116900130515     C*  Trasmissione solo a  una filiale
117000130515     C     V5CFLT        IFEQ      0
117100130515     C                   SETON                                        4028
117200130515     C                   MOVEL     ERR(15)       $MSG
117300130515     C                   GOTO      FINCT5
117400130515     C                   ELSE
117500130515     C                   Z-ADD     V5CFLT        ORGFIL
117600130515     C     ORGFIL        CHAIN     AZORG01L
117700130515     C                   IF        not %Found(azorg01l)
117800130515     C                   SETON                                        4028
117900130515     C                   MOVEL     ERR(16)       $MSG
118000130515     C                   GOTO      FINCT5
118100130515     C                   END
118200130515     C                   END
118300130515     C*  Trasmissione a tutti o a nessuno
118400130515     C                   ELSE
118500130515     C     V5CFLT        IFNE      0
118600130515     C                   SETON                                        4028
118700130515     C                   MOVEL     ERR(17)       $MSG
118800130515     C                   GOTO      FINCT5
118900130515     C                   END
119000130515     C                   END
119100130515     C*
119200130515     C     FINCT5        ENDSR
119300130515     C*---------------------------------------------------------------*
119400130515     C*  Duplica UNB x Partner con + linee x + nazioni                *
119500130515     C*---------------------------------------------------------------*
119600130515     C     Wind_Duplica  Begsr
119700130515     C*
119800130515     C* Salva la chiave da cui generare la duplica
119900130515     C                   MOVEL     V2CKEY        savKey_UNB       35
120000130515     C*
120100130515     c                   clear                   tc93w01
120200130515     C                   MOVEL     V2CKEY        w1unb1
120300130515     C*
120400130515     c     window_01     tag
120500130515     c                   exfmt     tc93w01
120600130515     c                   setoff                                       99
120700130515     C* Annulla Duplica
120800130515     C     *INKC         IFEQ      '1'
120900130515     C                   MOVEL     'S'           WFINE
121000130515     c                   end
121100130515     c   Kc
121200130515     cor Kl              goto      end_w01
121300130515     C*
121400130515     C*    controlli campi chiave
121500130515     C                   exsr      contr_W01
121600130515     c  nKf
121700130515     cor 99              goto      window_01
121800130515     C*
121900130515     C* Esegue  Duplica
122000130515     c   Kf              exsr      duplica_UNB
122100130515     C*
122200130515     C     end_w01       EndSR
122300130515     C*---------------------------------------------------------------*
122400130515     C*  contr_W01 -  Controlli sui campi  principali                 *
122500130515     C*---------------------------------------------------------------*
122600130515     C     contr_W01     BegSR
122700130515     C*
122800130515     C* Codice nazione x nuovo UNB
122900130515     C                   MOVEL     'PT'          KCOD
123000130515     C                   MOVEL     w1unb1        codice35         35
123100130515     C                   MOVE      w1unb2        codice35
123200130515     C                   MOVEL     codice35      Kkey
123300130515     C     KTAB          chain(n)  EDTAB01L
123400130515     c                   if        %Found(EDTAB01L)
123500130515     c                   seton                                        9951
123600130515     c                   goto      err_w01
123700130515     c                   end
123800130515     c                   if        w1unb2 = *blank
123900130515     c                   seton                                        9953
124000130515     c                   goto      err_w01
124100130515     c                   end
124200130515     C*
124300130515     C* Linea obbligatoria
124400130515     c                   if        w1clnp = *zeros
124500130515     c                   seton                                        9955
124600130515     c                   goto      err_w01
124700130515     c                   end
124800130515     C*
124900130515     C* Solo se un Partner controllo la linea che non deve essere già
125000130515     C*  stata immessa.
125100130515     c                   if        §puTPC = 'P' and w1clnp > 0
125200130515     c     w1clnp        lookup    PTlnp                                  52
125300130515     c                   if        *in52 = *on
125400130515     c                   seton                                        9952
125500130515     c                   goto      err_w01
125600130515     c                   end
125700130515     c                   end
125800130515     C*
125900130515     C* Nazione
126000130515     c                   if        w1cnaz = *blank
126100130515     c                   seton                                        9957
126200130515     c                   goto      err_w01
126300130515     c                   end
126400130515     C*
126500130515     C     err_w01       EndSR
126600130515     C*---------------------------------------------------------------*
126700130515     C*  duplica   -  le tabelle                                      *
126800130515     C*---------------------------------------------------------------*
126900130515     C     duplica_UNB   BegSR
127000130515     C*
127100130515     C*  Gestisco scrittura/aggiornamento key
127200130515     C*
127300130515     C                   MOVEL     *BLANKS       KKEY
127400130515     C                   MOVEL     V2CKEY        KKEY
127500130515     C                   MOVEL     'PT'          KCOD
127600130515     C     KTAB          CHAIN     EDTAB01L
127700130515     c                   if        %Found(EDTAB01L)
127800130515     C                   MOVEL     TABUNI        DSPT
127900130515      *
128000130515      * imposta i campi di scrittura PT
128100130515     c                   clear                   edtab000
128200130515     c                   if        w1cksc > 0
128300130515     c                   movel     w1cksc        §ptKSC
128400130515     c                   end
128500130515     c                   movel     w1clnp        §ptLNP
128600130515     c                   movel     w1cNAZ        §ptNAZ
128700130515     C                   MOVEL     DSPT          TABUNI
128800130515     C                   MOVEL     'PT'          TABCOD
128900130515     C                   MOVEL     codice35      TABKEY
129000130515     c                   write     edtab000
129100130515     c                   end
129200130515     C*
129300130515     C                   MOVEL     'PU'          KCOD
129400130515     C     KTAB          CHAIN     EDTAB01L
129500130515     c                   if        %Found(EDTAB01L)
129600130515     C                   MOVEL     TABUNI        DSPU
129700140221     c                   end
129800130515      *
129900130515      * imposta i campi di scrittura PU
130000130515     c                   clear                   edtab000
130100130515     C                   MOVEL     DSPU          TABUNI
130200130515     C                   MOVEL     'PU'          TABCOD
130300130515     C                   MOVEL     codice35      TABKEY
130400130515     c                   write     edtab000
130500140221     c*********          end
130600130515     C*
130700130515     C                   MOVEL     'PV'          KCOD
130800130515     C     KTAB          CHAIN     EDTAB01L
130900130515     c                   if        %Found(EDTAB01L)
131000130515     C                   MOVEL     TABUNI        DSPV
131100140221     c                   end
131200130515      *
131300130515      * imposta i campi di scrittura PV
131400130515     c                   clear                   edtab000
131500130515     C                   MOVEL     DSPV          TABUNI
131600130515     C                   MOVEL     'PV'          TABCOD
131700130515     C                   MOVEL     codice35      TABKEY
131800130515     c                   write     edtab000
131900140221     c*********          end
132000130515     C*
132100130515     C                   MOVEL     'PW'          KCOD
132200130515     C     KTAB          CHAIN     EDTAB01L
132300130515     c                   if        %Found(EDTAB01L)
132400130515     C                   MOVEL     TABUNI        DSPW
132500140221     c                   end
132600130515      *
132700130515      * imposta i campi di scrittura PW
132800130515     c                   clear                   edtab000
132900130515     C                   MOVEL     DSPW          TABUNI
133000130515     C                   MOVEL     'PW'          TABCOD
133100130515     C                   MOVEL     codice35      TABKEY
133200130515     c                   write     edtab000
133300140221     c*********          end
133400130515     C*
133500130515     C                   MOVEL     'PZ'          KCOD
133600130515     C     KTAB          CHAIN     EDTAB01L
133700130515     c                   if        %Found(EDTAB01L)
133800130515     C                   MOVEL     TABUNI        DSPZ
133900140221     c                   end
134000130515     C*
134100130515      * imposta i campi di scrittura PZ
134200130515     c                   clear                   edtab000
134300130515     C                   MOVEL     DSPZ          TABUNI
134400130515     C                   MOVEL     'PZ'          TABCOD
134500130515     C                   MOVEL     codice35      TABKEY
134600130515     c                   write     edtab000
134700140221     c*********          end
134800140221     C*
134900140221     C                   MOVEL     'PS'          KCOD
135000140221     C     KTAB          CHAIN     EDTAB01L
135100140221     c                   if        %Found(EDTAB01L)
135200140221     C                   MOVEL     TABUNI        DSPS
135300140221     c                   end
135400140221     C*
135500140221      * imposta i campi di scrittura PS
135600140221     c                   clear                   edtab000
135700140221     C                   MOVEL     DSPS          TABUNI
135800140221     C                   MOVEL     'PS'          TABCOD
135900140221     C                   MOVEL     codice35      TABKEY
136000140221     c                   write     edtab000
136100140221     c*********          end
136200130515     C*
136300130515     C                   MOVEL     'PO'          KCOD
136400130515     C     KTAB          CHAIN     EDTAB01L
136500130515     c                   if        %Found(EDTAB01L)
136600130515     C                   MOVEL     TABUNI        DSPO
136700140221     c                   end
136800130515     C*
136900130515      * imposta i campi di scrittura PO
137000130515     c                   clear                   edtab000
137100130515     C                   MOVEL     DSPO          TABUNI
137200130515     C                   MOVEL     'PO'          TABCOD
137300130515     C                   MOVEL     codice35      TABKEY
137400130515     c                   write     edtab000
137500140221     c*********          end
137600130515     C*
137700130515     C*  NON RIPORTA LE NOTE in DUPLICA
137800130515     C* Mostra la videata con il nuovo UNB da modificare in altri campi
137900130515     C                   MOVEL     codice35      v1ckey
138000130515     C                   MOVEL     '1'           $GESD             1
138100130515     C*
138200130515     C                   EndSR
138300130515     C*---------------------------------------------------------------*
138400130515     C*  INZ06: Inizializzo SESTA videata
138500130515     C*---------------------------------------------------------------*
138600130515     C     INZ06         BEGSR
138700130515     C*
138800130515     C                   SETOFF                                       404142
138900130515     C                   SETOFF                                       434428
139000130515     C                   SETOFF                                       49
139100130515     C                   MOVEL     '6'           $GESD
139200130515     C                   MOVEL     *BLANKS       $MSG
139300130515     C                   MOVEL     NOMPGM        V6CPGM
139400130515     C                   MOVEL     V2CKEY        V6CKEY
139500150112     C                   clear                   V6CAPP
139600150112     C                   clear                   V6Crcu
139700160706     C                   MOVEL     'S'           V6ftxnad
139800161013     C                   clear                   V6nadCN
139900161013     C                   clear                   V6cnsu21
140000130515      *
140100130515     c                   if          trovata_PT = 'S'
140200130515     C                   MOVEL     §PTCM1        V6CCM1
140300130515     C                   MOVEL     §PTNF1        V6CNF1
140400130515     C                   MOVEL     §PTDET        V6CDET
140500130515     C                   MOVEl     §PTTRD        V6CTRD
140600130515     C                   MOVEl     §PTSGN        V6CRMN
140700130515      ****
140800130515     c                   if          trovata_PU = 'S'
140900130515     C                   Z-ADD     §PUEL2        V6CEL2
141000130515     C                   Z-ADD     §PULP2        V6CLP2
141100130515     C                   Z-ADD     §PULA2        V6CLA2
141200130515     C                   Z-ADD     §PUNR2        V6CNR2
141300130515     C                   Z-ADD     §PUSG2        V6CSG2
141400130515     C                   Z-ADD     §PUZN2        V6CZN2
141500130515     C                   MOVEL     §PULAB        V6CLAB
141600130515     C                   MOVEL     §PURFF        V6CRFF
141700130515     C                   MOVEL     §PUdate203    V6C203
141800130515     C                   MOVEL     §PUFIRMA      V6CFIRMA
141900130515     c                   end
142000140221      ****
142100140221     c                   if          trovata_PS = 'S'
142200140221     C                   MOVEL     §PSAPP        V6CAPP
142300140416     C                   MOVEL     §PSrcu        V6Crcu
142400160706     C                   MOVEL     §PSftxnad     V6ftxnad
142500161013     C                   MOVEL     §PSNADCN      V6nadCN
142600161013     C                   MOVEL     §PScnsu21     V6cnsu21
142700140221     c                   end
142800130515      ****
142900130515     C                   Else
143000130515     C*
143100130515     C                   Z-ADD     1             V6CEL2
143200130515     C                   END
143300130515     C*
143400130515     C                   ENDSR
143500960911     C*---------------------------------------------------------------*
143600960911     C*  GESD06: Gestione sesta videata
143700960911     C*---------------------------------------------------------------*
143800960911     C     GESD06        BEGSR
143900960911     C*
144000111207     C*  in reverse se si Inviano STATUS diversi dai nostri standard
144100111207     c                   setoff                                       51
144200111207     c                   if        v6CLAB ='S'
144300111207     c                   seton                                        51
144400111207     c                   end
144500111207     C*
144600960911     C                   EXFMT     TC93D06
144700960911     C                   SETOFF                                       404142
144800961011     C                   SETOFF                                       434445
144900961021     C                   SETOFF                                       434428
145000050831     C                   SETOFF                                       49
145100110913      *  Note ESPLICATIVE
145200110913     C     *INKB         IFEQ      '1'
145300110913     c                   exsr      winNOTE
145400110913     C                   GOTO      FINVD6
145500110913     C                   END
145600960911     C*  Fine Lavoro
145700960911     C     *INKC         IFEQ      '1'
145800960911     C                   MOVEL     'S'           WFINE
145900960911     C                   GOTO      FINVD6
146000960911     C                   END
146100960911     C*  Ritorno
146200960911     C     *INKL         IFEQ      '1'
146300130509     C                   MOVEL     '3'           $GESD
146400960911     C                   GOTO      FINVD6
146500960911     C                   END
146600960911     C*  Annullamento
146700960911     C     *INKQ         IFEQ      '1'
146800960911     C                   EXSR      ANNUL
146900960911     C                   GOTO      FINVD6
147000960911     C                   END
147100960911     C*  Controlli
147200960911     C                   EXSR      CTR06
147300960911     C   28              GOTO      FINVD6
147400960911     C*  Ripristino
147500960911     C     *INKE         IFEQ      '1'
147600960911     C                   EXSR      RIPRIS
147700960911     C                   GOTO      FINVD6
147800960911     C                   END
147900960911     C*  Conferma
148000960911     C     *INKF         IFEQ      '1'
148100960911     C                   EXSR      CONFER
148200960911     C                   GOTO      FINVD6
148300960911     C                   END
148400961021     C*  Invio: SETTIMA videata
148500961021     C                   EXSR      INZ07
148600960911     C*
148700960911     C     FINVD6        ENDSR
148800130515     C*---------------------------------------------------------------*
148900130515     C*  CTR06: Controlli sesta videata
149000130515     C*---------------------------------------------------------------*
149100130515     C     CTR06         BEGSR
149200130515     C*
149300130515     C*  Se numero CMR da ritornare obbligatorio indicarlo
149400130515     C     V3CCMR        IFEQ      ' '
149500130515     C     V6CCM1        ANDEQ     'S'
149600130515     C                   SETON                                        4028
149700130515     C                   MOVEL     ERR(18)       $MSG
149800130515     C                   GOTO      FINCT6
149900130515     C                   END
150000130515     C*  Se numero foglio viaggio da ritornare obbligatorio indicarlo
150100130515     C     V3CNFV        IFEQ      ' '
150200130515     C     V6CNF1        ANDEQ     'S'
150300130515     C                   SETON                                        4128
150400130515     C                   MOVEL     ERR(19)       $MSG
150500130515     C                   GOTO      FINCT6
150600130515     C                   END
150700130515     C*  Se numero sgn. bartolini da ritornare obbligatorio indicarlo
150800130515     C     V2CBS1        IFEQ      ' '
150900130515     C     V6CDET        ANDNE     'M'
151000130515     C                   SETON                                        4228
151100130515     C                   MOVEL     ERR(22)       $MSG
151200130515     C                   GOTO      FINCT6
151300130515     C                   END
151400130515      ***
151500130515     C     V6CDET        IFne      'M'
151600130515     C     V6CEL2        ifEQ      0
151700130515     C     V6CEL2        orGT      10
151800130515     C                   SETON                                        2845
151900130515     C                   MOVEL     ERR(30)       $MSG
152000130515     C                   GOTO      FINCT6
152100130515     c                   end
152200130515     c                   end
152300130515      ***
152400130515     C*  Se non devo ritornare sgn. bartolini non devono essere
152500130515     C*  immessi nr.elementi-posizione iniz.lnp/lna/nrs/sgn/znc
152600140416     C*
152700130515     C     V6CDET        IFEQ      'M'
152800140416     C*
152900130515     C     V6CEL2        IFGT      0
153000130515     C                   SETON                                        284245
153100130515     C                   MOVEL     ERR(23)       $MSG
153200130515     C                   GOTO      FINCT6
153300130515     C                   END
153400130515      ***
153500130515     C     V6CLP2        IFGT      0
153600130515     C     V6CLA2        ORGT      0
153700130515     C     V6CNR2        ORGT      0
153800130515     C     V6CSG2        ORGT      0
153900130515     C     V6CZN2        ORGT      0
154000130515     C                   SETON                                        2840
154100130515     C                   MOVEL     ERR(23)       $MSG
154200130515     C                   GOTO      FINCT6
154300130515     C                   END
154400130515     C*
154500130515     C                   ELSE
154600140416     C*
154700130515     C     V2CBS1        IFEQ      'S'
154800140416     C*
154900130515     C     V6CEL2        IFEQ      0
155000140416     C                   SETON                                        284045
155100130515     C                   MOVEL     ERR(24)       $MSG
155200130515     C                   GOTO      FINCT6
155300130515     C                   END
155400140416      *
155500140416      *  prima erano in "AND" trasformate in "OR" perchè devono esserci tutte
155600140416      *   le definizioni del NOSTRO segnacollo
155700130515     C     V6CLP2        IFEQ      0
155800140416     C     V6CLA2        orEQ      0
155900140416     C     V6CNR2        orEQ      0
156000140416     C     V6CSG2        orEQ      0
156100140416     C     V6CZN2        orEQ      0
156200130515     C                   SETON                                        284042
156300130515     C                   MOVEL     ERR(24)       $MSG
156400130515     C                   GOTO      FINCT6
156500130515     C                   END
156600130515     C*
156700130515     C                   END
156800130515     C                   END
156900130515     C*
157000130515     C     FINCT6        ENDSR
157100130515     C*---------------------------------------------------------------*
157200130515     C*  INZ07: Inizializzo SETTIMA VIdeata
157300130515     C*---------------------------------------------------------------*
157400130515     C     INZ07         BEGSR
157500130515     C*
157600130515     C                   SETOFF                                       404142
157700130515     C                   SETOFF                                       4328
157800130515     C                   MOVEL     '7'           $GESD
157900130515     C                   MOVEL     *BLANKS       $MSG
158000130515     C                   MOVEL     NOMPGM        V7CPGM
158100130515     C                   MOVEL     V2CKEY        V7CKEY
158200130515      ****
158300130515     c                   if          trovata_PT = 'S'
158400130515      ****
158500130515     c                   if          trovata_PU = 'S'
158600130515     C                   MOVEL     §PUDTA        V7CDTA
158700130515     C                   MOVEL     §PUECC        V7CECC
158800130515     C                   MOVEL     §PUDTB        V7CDTB
158900130515     C                   MOVEL     §PUSGD        V7CSGD
159000130515     C                   MOVEL     §PUATR        V7CATR
159100130515     c                   end
159200130515      *
159300130515     c                   if          trovata_PV = 'S'
159400130515     C                   MOVEL     §PVCNVSTS     V7CCNVSTS
159500130515     C                   MOVEL     §PVIDENT      V7CIDEnt
159600130515     c                   end
159700130515      *
159800130515     C                   ELSE
159900130515     C                   MOVEL     'M'           V7CSGD
160000130515     C                   END
160100130515     C*
160200130515     C                   ENDSR
160300961021     C*---------------------------------------------------------------*
160400961021     C*  GESD07: Gestione settima videata
160500961021     C*---------------------------------------------------------------*
160600961021     C     GESD07        BEGSR
160700961021     C*
160800111221     c     V7CCNVSTS     comp      'S'                                    77
160900961021     C                   EXFMT     TC93D07
161000961021     C                   SETOFF                                       404142
161100961021     C                   SETOFF                                       4328
161200110913      *  Note ESPLICATIVE
161300110913     C     *INKB         IFEQ      '1'
161400110913     c                   exsr      winNOTE
161500110913     C                   GOTO      FINVD7
161600110913     C                   END
161700961021     C*  Fine Lavoro
161800961021     C     *INKC         IFEQ      '1'
161900961021     C                   MOVEL     'S'           WFINE
162000961021     C                   GOTO      FINVD7
162100961021     C                   END
162200961021     C*  Ritorno
162300961021     C     *INKL         IFEQ      '1'
162400130509     C                   MOVEL     '6'           $GESD
162500961021     C                   GOTO      FINVD7
162600961021     C                   END
162700961021     C*  Annullamento
162800961021     C     *INKQ         IFEQ      '1'
162900961021     C                   EXSR      ANNUL
163000961021     C                   GOTO      FINVD7
163100961021     C                   END
163200961021     C*  Controlli
163300961021     C                   EXSR      CTR07
163400961021     C   28              GOTO      FINVD7
163500961021     C*  Ripristino
163600961021     C     *INKE         IFEQ      '1'
163700961021     C                   EXSR      RIPRIS
163800961021     C                   GOTO      FINVD7
163900961021     C                   END
164000961021     C*  Conferma
164100961021     C     *INKF         IFEQ      '1'
164200961021     C                   EXSR      CONFER
164300961021     C                   GOTO      FINVD7
164400961021     C                   END
164500111221     C*
164600111221     C*  Prima di passare Oltre se è attivata la funzione
164700111221     C*   di ricezione IFTSTA con codici diversi dal nostro Standard
164800111221     C*    apre il programma per poterli codificare sul file EDSTS00F
164900111221     C*     in modo da avere tutto correlato.
165000111221     c     V7CCNVSTS     ifeq      'S'
165100111221     C                   EXSR      OPEN_SR1
165200111221     c                   end
165300111221     C*
165400961021     C*  Invio: QUARTA videata
165500961021     C                   EXSR      INZ04
165600961021     C*
165700961021     C     FINVD7        ENDSR
165800130515     C*---------------------------------------------------------------*
165900130515     C*  CTR07: Controlli settima videata
166000130515     C*---------------------------------------------------------------*
166100130515     C     CTR07         BEGSR
166200130515     C*
166300130515     C*  Se non trasmetto date arrivo no eccedenze
166400130515     C     V7CDTA        IFEQ      *BLANKS
166500130515     C     V7CECC        ANDNE     *BLANKS
166600130515     C                   SETON                                        284041
166700130515     C                   MOVEL     ERR(25)       $MSG
166800130515     C                   GOTO      FINCT7
166900130515     C                   END
167000130515     C*  Se non trasmetto date arrivo no bolle positive
167100130515     C     V7CDTA        IFEQ      *BLANKS
167200130515     C     V7CDTB        ANDNE     *BLANKS
167300130515     C                   SETON                                        284042
167400130515     C                   MOVEL     ERR(26)       $MSG
167500130515     C                   GOTO      FINCT7
167600130515     C                   END
167700130515     C*  Se non trasmetto date arrivo no sgn
167800130515     C     V7CDTA        IFEQ      *BLANKS
167900130515     C     V7CSGD        ANDNE     'M'
168000130515     C                   SETON                                        284043
168100130515     C                   MOVEL     ERR(27)       $MSG
168200130515     C                   GOTO      FINCT7
168300130515     C                   END
168400130515     C*
168500130515     C     FINCT7        ENDSR
168600130515     C*---------------------------------------------------------------*
168700130515     C*  INZ08: Inizializzo OTTAVA  VIdeata
168800130515     C*---------------------------------------------------------------*
168900130515     C     INZ08         BEGSR
169000130515     C*
169100130515     C                   SETOFF                                       414243
169200130515     C                   SETOFF                                       28
169300130515     C                   MOVEL     '8'           $GESD
169400130515     C                   MOVEL     *BLANKS       $MSG
169500130515     C                   MOVEL     NOMPGM        V8CPGM
169600130515     C                   MOVEL     V2CKEY        V8CKEY
169700130515     C*
169800130515     c                   if          trovata_PT = 'S'
169900130515     C*
170000130515     c                   if          trovata_PV = 'S'
170100130515     C                   MOVEL     §PVRFF        V8CRFF
170200130515     C                   MOVEL     §PVRFS        V8CRFS
170300130515     C                   MOVEL     §PVRSE        V8CRSE
170400130515     C                   MOVEL     §PVRCU        V8CRCU
170500130515     C                   MOVEL     §PVpes        V8Cpes
170600130515     C                   MOVEL     §PVcol        V8Ccol
170700130515     C                   MOVEL     §PVsin        V8Csin
170800130515     C                   MOVEL     §PVicn        V8Cicn
170900130515     C                   MOVEL     §PV2in        V8C2in
171000130515     C                   MOVEL     §PVinv        V8Cinv
171100130515     C                   MOVEL     §PVcniRMN     V8Ccni
171200130515     C                   END
171300130515     C*
171400130515     C                   END
171500130515     C*
171600130515     C                   ENDSR
171700070131     C*---------------------------------------------------------------*
171800070131     C*  GESD08: Gestione ottava videata
171900070131     C*---------------------------------------------------------------*
172000070131     C     GESD08        BEGSR
172100070131     C*
172200070131     C                   EXFMT     TC93D08
172300070131     C                   SETOFF                                       28
172400110913      *  Note ESPLICATIVE
172500110913     C     *INKB         IFEQ      '1'
172600110913     c                   exsr      winNOTE
172700110913     C                   GOTO      FINVD8
172800110913     C                   END
172900070131     C*  Fine Lavoro
173000070131     C     *INKC         IFEQ      '1'
173100070131     C                   MOVEL     'S'           WFINE
173200070131     C                   GOTO      FINVD8
173300070131     C                   END
173400070131     C*  Ritorno
173500070131     C     *INKL         IFEQ      '1'
173600130509     C                   MOVEL     '4'           $GESD
173700070131     C                   GOTO      FINVD8
173800070131     C                   END
173900070131     C*  Annullamento
174000070131     C     *INKQ         IFEQ      '1'
174100070131     C                   EXSR      ANNUL
174200070131     C                   GOTO      FINVD8
174300070131     C                   END
174400070131     C*  Controlli
174500070131     C                   EXSR      CTR08
174600070131     C   28              GOTO      FINVD8
174700070131     C*  Ripristino
174800070131     C     *INKE         IFEQ      '1'
174900070131     C                   EXSR      RIPRIS
175000070131     C                   GOTO      FINVD8
175100070131     C                   END
175200070131     C*  Conferma
175300070131     C     *INKF         IFEQ      '1'
175400070131     C                   EXSR      CONFER
175500070131     C                   GOTO      FINVD8
175600070131     C                   END
175700101019     C*  Invio: videata di riepilogo
175800101019     C                   EXSR      INZriep
175900070131     C*
176000070131     C     FINVD8        ENDSR
176100130515     C*---------------------------------------------------------------*
176200130515     C*  CTR08: Controlli ottava videata
176300130515     C*---------------------------------------------------------------*
176400130515     C     CTR08         BEGSR
176500130515     C*
176600130515     C                   SETOff                                       4128
176700130515     c                   clear                   ult_char          3 0
176800130515     c                   eval      ult_char = %len(%trimr(V8EMAI))
176900130515     C*
177000130515     C*  gli indirizzi immessi devono finire con (;) separatore
177100130515     c                   if        ult_char > 0
177200130515     C                   IF        %subst(v8emai:ult_char:1) <> ';'
177300130515     C                   SETON                                        4128
177400130515     C                   MOVEL     ERR(31)       $MSG
177500130515     C                   GOTO      FINCT8
177600130515     C                   END
177700130515     c                   end
177800130515     C*
177900130515     C     FINCT8        ENDSR
178000130515     C*---------------------------------------------------------------*
178100130515     C*  INZriep Inizializzo VIdeata dati riepilogativi
178200130515     C*---------------------------------------------------------------*
178300130515     C     INZriep       BEGSR
178400130515     C*
178500130515     C                   SETOFF                                       414243
178600130515     C                   SETOFF                                       2866
178700130515     C*
178800130515     C                   MOVEL     'P'           $GESD
178900130515     C                   MOVEL     NOMPGM        VPTNPGM
179000130515     C                   MOVEL     *BLANKS       $MSG
179100130515     C*
179200130515     C                   MOVEL     v2ckey        VPTNUNB
179300130515     C                   MOVEL     V2cKSC        VPTNcli
179400130515     C                   clear                   VPTNCOM
179500130515     C                   clear                   VPTNMAIL
179600130515     c                   z-add     0             VPT_3KKSU
179700130522     c                   clear                   VPOsndPRTF
179800130515     c                   clear                   v2trsm
179900130515     C*
180000130515     C                   clear                   VRCZFB
180100130515     C                   clear                   VRCZTB
180200130515     C                   clear                   VRCZPB
180300130515     C                   clear                   VTRSFB
180400130515     C                   clear                   VTRSPB
180500130515     C                   clear                   VTRSTB
180600130515     C                   clear                   VRCZFS
180700130515     C                   clear                   VRCZPS
180800130515     C                   clear                   VRCZTS
180900130515     C                   clear                   VTRSFS
181000130515     C                   clear                   VTRSPS
181100130515     C                   clear                   VTRSTS
181200130515     C                   clear                   VRCZFO
181300130515     C                   clear                   VRCZPO
181400130515     C                   clear                   VRCZTO
181500130515     C                   clear                   VTRSFO
181600130515     C                   clear                   VTRSPO
181700130515     C                   clear                   VTRSTO
181800130515      *---
181900130515     C                   clear                   ORCZFB
182000130515     C                   clear                   ORCZPB
182100130515     C                   clear                   ORCZTB
182200130515     C                   clear                   OTRSFB
182300130515     C                   clear                   OTRSPB
182400130515     C                   clear                   OTRSTB
182500130515     C                   clear                   ORCZFS
182600130515     C                   clear                   ORCZPS
182700130515     C                   clear                   ORCZTS
182800130515     C                   clear                   OTRSFS
182900130515     C                   clear                   OTRSPS
183000130515     C                   clear                   OTRSTS
183100130515     C                   clear                   ORCZFO
183200130515     C                   clear                   ORCZPO
183300130515     C                   clear                   ORCZTO
183400130515     C                   clear                   OTRSFO
183500130515     C                   clear                   OTRSPO
183600130515     C                   clear                   OTRSTO
183700130515      *
183800130515     c                   if        v3cwrk = 'S'
183900130515     C                   MOVEL     'SI'          VPTNCMR
184000130515     c                   else
184100130515     C                   MOVEL     'NO'          VPTNCMR
184200130515     c                   end
184300130515     C*
184400130515     C*  Controlla subito se strutturalmente vi sono delle parti del msg EDI
184500130515     C*  DEFINITE diversamente dallo standard
184600130515     C*  se ci sono dei campi da tradurre in modo diverso devono essere definiti
184700130515     C*  nel file tabella EDI EDSTBL00F ..... quindi imposta una dicitura a video
184800130515     C*  a pro-memoria accendendo l'ind.66
184900130515     C     V2cKEY        setll     edstbl01l
185000130515     c                   if        %equal
185100130515     c                   seton                                        66
185200130515     c                   end
185300130515     C*
185400130515     C                   move      '1'           tblkut
185500130515     C                   move      '3K'          tblcod
185600130515     C                   movel(p)  V2cKSC        tblkey
185700130515     C     Ktabel        chain     tabel00f
185800130515     c                   if        %Found(tabel00f)
185900130515     c                   eval      ds3k = tblUNI
186000130515     c                   z-add     §3KCKS        VPT_3KKSU
186100130515     c                   end
186200130515     C*
186300130515     c                   if          trovata_PT = 'S'
186400130515     C*
186500130515     c                   if          trovata_PU = 'S'
186600130515     C                   MOVEL     §puXXX        VPTxxx
186700130515     c                   if        §puXXX = 'S'
186800130515     c                   eval      v2trsm = 'ON'
186900130515     c                   end
187000130515     c                   end
187100130515      *
187200130515     c                   if         trovata_PW  = 'S'
187300130515     C                   MOVEL     §PWCOMMER     VPTNCOM
187400130515     C                   MOVEL     §PWCONTACT    VPTNMAIL
187500130515     c                   end
187600130515      *
187700130515     c                   if         trovata_PZ  = 'S'
187800130515      *
187900130515     C                   MOVEL     §PZTIPRCZB    VRCZTB
188000130515     C                   MOVEL     §PZFILRCZB    VRCZFB
188100130515     C                   MOVEL     §PZPGMRCZB    VRCZPB
188200130515     C                   MOVEL     §PZTIPTRSB    VTRSTB
188300130515     C                   MOVEL     §PZFILTRSB    VTRSFB
188400130515     C                   MOVEL     §PZPGMTRSB    VTRSPB
188500130515     C                   MOVEL     §PZTIPRCZS    VRCZTS
188600130515     C                   MOVEL     §PZFILRCZS    VRCZFS
188700130515     C                   MOVEL     §PZPGMRCZS    VRCZPS
188800130515     C                   MOVEL     §PZTIPTRSS    VTRSTS
188900130515     C                   MOVEL     §PZFILTRSS    VTRSFS
189000130515     C                   MOVEL     §PZPGMTRSS    VTRSPS
189100130515     C                   MOVEL     §PZFTPCOD     VPTCONNEX
189200130515      *---
189300130515     c                   end
189400130515      *
189500130515     c                   if         trovata_PO  = 'S'
189600130515      *
189700130515     C                   MOVEL     §PoTIPRCZo    VRCZTO
189800130515     C                   MOVEL     §PoFILRCZo    VRCZFO
189900130515     C                   MOVEL     §PoPGMRCZo    VRCZPO
190000130515     C                   MOVEL     §PoTIPTRSo    VTRSTO
190100130515     C                   MOVEL     §PoFILTRSo    VTRSFO
190200130515     C                   MOVEL     §PoPGMTRSo    VTRSPO
190300130522     C                   MOVEL     §PoSNDprtf    VPOsndPRTF
190400130515      *---
190500130515     c                   end
190600130515      *---
190700130515     c                   if         trovata_PZ  = 'S' or
190800130515     c                              trovata_PO  = 'S'
190900130515     c                   exsr      ctrvRIE
191000130515     c                   end
191100130515      *---
191200130515     c                   end
191300130515      *
191400130515     C                   ENDSR
191500101001     C*---------------------------------------------------------------*
191600101019     C*  GESriep: Gestione Dati di riepilogo
191700101001     C*---------------------------------------------------------------*
191800101019     C     GESriep       BEGSR
191900101001     C*
192000101001     C                   EXFMT     TC93PTN
192100101001     C                   SETOFF                                       28
192200110913      *  Note ESPLICATIVE
192300110913     C     *INKB         IFEQ      '1'
192400110913     c                   exsr      winNOTE
192500110913     C                   GOTO      FINVrie
192600110913     C                   END
192700101001     C*  Fine Lavoro
192800101001     C     *INKC         IFEQ      '1'
192900101001     C                   MOVEL     'S'           WFINE
193000101019     C                   GOTO      FINVrie
193100101001     C                   END
193200101001     C*  Ritorno
193300101001     C     *INKL         IFEQ      '1'
193400130509     C                   MOVEL     '8'           $GESD
193500101019     C                   GOTO      FINVrie
193600101001     C                   END
193700101001     C*  Annullamento
193800101001     C     *INKQ         IFEQ      '1'
193900101001     C                   EXSR      ANNUL
194000101019     C                   GOTO      FINVrie
194100101001     C                   END
194200101001     C*  Controlli
194300101019     C                   EXSR      CTRVrie
194400101019     C   28              GOTO      FINVrie
194500101001     C*  Ripristino
194600101001     C     *INKE         IFEQ      '1'
194700101001     C                   EXSR      RIPRIS
194800101019     C                   GOTO      FINVrie
194900101001     C                   END
195000101001     C*  Conferma
195100101001     C     *INKF         IFEQ      '1'
195200101001     C                   EXSR      CONFER
195300101019     C                   GOTO      FINVrie
195400101001     C                   END
195500101001     C*  Invio: Quinta  videata
195600101001     C                   EXSR      INZ05
195700130515     C*   Abilita l'F6
195800130515     C                   SETON                                        06
195900101001     C*
196000101019     C     FINVrie       ENDSR
196100101001     C*---------------------------------------------------------------*
196200101019     C*  CTRVrie Controlli videata di file riepilogativo
196300101001     C*---------------------------------------------------------------*
196400101019     C     CTRVrie       BEGSR
196500101001     C*
196600130412     C                   SETOff                                       28
196700101020     C                   SETOff                                       414243
196800101020     C                   SETOff                                       444546
196900101020     C                   SETOff                                       474849
197000101020     C                   SETOff                                       505152
197100130412     C                   SETOff                                       535455
197200130412     C                   SETOff                                       565758
197300130522     C                   SETOff                                       5960
197400101020     c                   clear                   oRCZFB
197500130412     c                   clear                   oRCZPB
197600130412     c                   clear                   oRCZtB
197700130412      *
197800101020     c                   clear                   oTRSFB
197900110608     c                   clear                   oTRSPB
198000130412     c                   clear                   oTRStB
198100130412      *
198200130412     c                   clear                   oRCZFS
198300130412     c                   clear                   oRCZPS
198400130412     c                   clear                   oRCZtS
198500130412      *
198600130412     c                   clear                   oTRSFS
198700130412     c                   clear                   oTRSPS
198800110608     c                   clear                   oTRStS
198900130412      *
199000130412     c                   clear                   oRCZFo
199100130412     c                   clear                   oRCZPo
199200130412     c                   clear                   oRCZto
199300130412      *
199400130412     c                   clear                   oTRSFo
199500130412     c                   clear                   oTRSPo
199600130412     c                   clear                   oTRSto
199700120416      **
199800120416      **  Codice di trasmissione vuoto e richiesta la trasmissione con questo codice
199900120416     c                   if        VPTCONNEX = 0 and VPTXXX ='S'
200000120416     c                   seton                                        53  28
200100120416     c                   goto      FINCTptn
200200120416     c                   end
200300120416      **
200400101020     C* file Bolle
200500101020     c     vRCZFB        ifne      *blank
200600101020     c                   z-add     1             Npr
200700101020     c     vRCZFB        lookup    pf_key(Npr)                            21
200800101020     c                   if        not *in21
200900101020     c                   seton                                        41  28
201000101020     c                   goto      FINCTptn
201100101020     c                   else
201200101020     c                   eval      oRCZFB = pf_DS(Npr)
201300101020     c                   end
201400101020     c                   end
201500101020     C*
201600101020     c     vTRSFB        ifne      *blank
201700101020     c                   z-add     1             Npr
201800101020     c     vTRSFB        lookup    pf_key(Npr)                            21
201900101020     c                   if        not *in21
202000101020     c                   seton                                        42  28
202100101020     c                   goto      FINCTptn
202200101020     c                   else
202300101020     c                   eval      oTRSFB = pf_DS(Npr)
202400101020     c                   end
202500101020     c                   end
202600101020     C* file Stati
202700101020     c     vRCZFS        ifne      *blank
202800101020     c                   z-add     1             Npr
202900101020     c     vRCZFS        lookup    pf_key(Npr)                            21
203000101020     c                   if        not *in21
203100101020     c                   seton                                        43  28
203200101020     c                   goto      FINCTptn
203300101020     c                   else
203400101020     c                   eval      oRCZFS = pf_DS(Npr)
203500101020     c                   end
203600101020     c                   end
203700101020     C*
203800101020     c     vTRSFS        ifne      *blank
203900101020     c                   z-add     1             Npr
204000101020     c     vTRSFS        lookup    pf_key(Npr)                            21
204100101020     c                   if        not *in21
204200101020     c                   seton                                        44  28
204300101020     c                   goto      FINCTptn
204400101020     c                   else
204500101020     c                   eval      oTRSFS = pf_DS(Npr)
204600101020     c                   end
204700101020     c                   end
204800130412     C* file ORM
204900130412     c     vRCZFo        ifne      *blank
205000130412     c                   z-add     1             Npr
205100130412     c     vRCZFo        lookup    pf_key(Npr)                            21
205200130412     c                   if        not *in21
205300130412     c                   seton                                        54  28
205400130412     c                   goto      FINCTptn
205500130412     c                   else
205600130412     c                   eval      oRCZFo = pf_DS(Npr)
205700130412     c                   end
205800130412     c                   end
205900130412     C*
206000130412     c     vTRSFo        ifne      *blank
206100130412     c                   z-add     1             Npr
206200130412     c     vTRSFo        lookup    pf_key(Npr)                            21
206300130412     c                   if        not *in21
206400130412     c                   seton                                        55  28
206500130412     c                   goto      FINCTptn
206600130412     c                   else
206700130412     c                   eval      oTRSFo = pf_DS(Npr)
206800130412     c                   end
206900130412     c                   end
207000110608     C*                                 0
207100101020     C* pgm  Bolle
207200101020     c     vRCZpB        ifne      *blank
207300101020     c                   z-add     1             Npr
207400101020     c     vRCZpB        lookup    pr_key(Npr)                            21
207500101020     c                   if        not *in21
207600101020     c                   seton                                        45  28
207700101020     c                   goto      FINCTptn
207800101020     c                   else
207900110322     c                   eval      oRCZpB = pr_DS(Npr)
208000110322     c                   end
208100101020     c                   end
208200101020     C*
208300101020     c     vTRSpB        ifne      *blank
208400101020     c                   z-add     1             Npr
208500101020     c     vTRSpB        lookup    pr_key(Npr)                            21
208600101020     c                   if        not *in21
208700101020     c                   seton                                        46  28
208800101020     c                   goto      FINCTptn
208900101020     c                   else
209000110608     c                   eval      dsPR   = pr_DS(Npr)
209100110608     c                   eval      oTRSpB = §PRtraDEX
209200101020     c                   end
209300101020     c                   end
209400130412     C* pgm  Stati
209500101020     c     vRCZpS        ifne      *blank
209600101020     c                   z-add     1             Npr
209700101020     c     vRCZpS        lookup    pr_key(Npr)                            21
209800101020     c                   if        not *in21
209900101020     c                   seton                                        47  28
210000101020     c                   goto      FINCTptn
210100101020     c                   else
210200101020     c                   eval      oRCZpS = pr_DS(Npr)
210300101020     c                   end
210400101020     c                   end
210500101020     C*
210600101020     c     vTRSpS        ifne      *blank
210700101020     c                   z-add     1             Npr
210800101020     c     vTRSpS        lookup    pr_key(Npr)                            21
210900101020     c                   if        not *in21
211000101020     c                   seton                                        48  28
211100101020     c                   goto      FINCTptn
211200101020     c                   else
211300110322     c                   eval      DSpr   = pr_DS(Npr)
211400110322     c                   eval      oTRSpS = §PRTRADEX
211500101020     c                   end
211600101020     c                   end
211700130412     C* pgm  ORM
211800130412     c     vRCZpO        ifne      *blank
211900130412     c                   z-add     1             Npr
212000130412     c     vRCZpO        lookup    pr_key(Npr)                            21
212100130412     c                   if        not *in21
212200130412     c                   seton                                        58  28
212300130412     c                   goto      FINCTptn
212400130412     c                   else
212500130412     c                   eval      oRCZpO = pr_DS(Npr)
212600130412     c                   end
212700130412     c                   end
212800130412     C*
212900130412     c     vTRSpO        ifne      *blank
213000130412     c                   z-add     1             Npr
213100130412     c     vTRSpO        lookup    pr_key(Npr)                            21
213200130412     c                   if        not *in21
213300130412     c                   seton                                        59  28
213400130412     c                   goto      FINCTptn
213500130412     c                   else
213600130412     c                   eval      DSpr   = pr_DS(Npr)
213700130412     c                   eval      oTRSpO = §PRTRADEX
213800130412     c                   end
213900130412     c                   end
214000101020     C*
214100130522     C*   se Non c'è il programma di traduzione il flag di Invio LISTA di Controllo
214200130522     C*   parallela x gli ORM NON deve esserci
214300130522     c                   if        vTRSpO = *blank
214400130522     c                                       and VPOsndPRTF <> *blank
214500130522     c                   seton                                        60  28
214600130522     c                   goto      FINCTptn
214700130522     c                   end
214800130522     C*
214900101020     C* tipo invio Bolle
215000101020     c     vRCZtB        ifne      *blank
215100101020     c                   z-add     1             Npr
215200101020     c     vRCZtB        lookup    pi_key(Npr)                            21
215300101020     c                   if        not *in21
215400101020     c                   seton                                        49  28
215500101020     c                   goto      FINCTptn
215600101020     c                   else
215700101020     c                   eval      oRCZtB = pi_DS(Npr)
215800101020     c                   end
215900101020     c                   end
216000101020     C*
216100101020     c     vTRStB        ifne      *blank
216200101020     c                   z-add     1             Npr
216300101020     c     vTRStB        lookup    pi_key(Npr)                            21
216400101020     c                   if        not *in21
216500101020     c                   seton                                        50  28
216600101020     c                   goto      FINCTptn
216700101020     c                   else
216800101020     c                   eval      oTRStB = pi_DS(Npr)
216900101020     c                   end
217000101020     c                   end
217100101020     C* tipo invio Stati
217200101020     c     vRCZtS        ifne      *blank
217300101020     c                   z-add     1             Npr
217400101020     c     vRCZtS        lookup    pi_key(Npr)                            21
217500101020     c                   if        not *in21
217600101020     c                   seton                                        51  28
217700101020     c                   goto      FINCTptn
217800101020     c                   else
217900101020     c                   eval      oRCZtS = pi_DS(Npr)
218000101020     c                   end
218100101020     c                   end
218200101020     C*
218300101020     c     vTRStS        ifne      *blank
218400101020     c                   z-add     1             Npr
218500101020     c     vTRStS        lookup    pi_key(Npr)                            21
218600101020     c                   if        not *in21
218700101020     c                   seton                                        52  28
218800101020     c                   goto      FINCTptn
218900101020     c                   else
219000101020     c                   eval      oTRStS = pi_DS(Npr)
219100101020     c                   end
219200101020     c                   end
219300130412     C* tipo invio ORM
219400130412     c     vRCZtO        ifne      *blank
219500130412     c                   z-add     1             Npr
219600130412     c     vRCZtO        lookup    pi_key(Npr)                            21
219700130412     c                   if        not *in21
219800130412     c                   seton                                        56  28
219900130412     c                   goto      FINCTptn
220000130412     c                   else
220100130412     c                   eval      oRCZtO = pi_DS(Npr)
220200130412     c                   end
220300130412     c                   end
220400130412     C*
220500130412     c     vTRStO        ifne      *blank
220600130412     c                   z-add     1             Npr
220700130412     c     vTRStO        lookup    pi_key(Npr)                            21
220800130412     c                   if        not *in21
220900130412     c                   seton                                        57  28
221000130412     c                   goto      FINCTptn
221100130412     c                   else
221200130412     c                   eval      oTRStO = pi_DS(Npr)
221300130412     c                   end
221400130412     c                   end
221500101020     C*
221600101001     C*
221700101001     C     FINCTptn      ENDSR
221800960607     C*---------------------------------------------------------------*
221900960607     C*  ANNUL: Eseguo annullamento tabella                           *
222000960607     C*---------------------------------------------------------------*
222100960607     C     ANNUL         BEGSR
222200130509     C*
222300130509     c                   eval      Operazione = Annulla
222400130509     c                   exsr      AGGTabella
222500960607     C*
222600960607     C                   ENDSR
222700960607     C*---------------------------------------------------------------*
222800960607     C*  RIPRIS: Ripristino tabella annullata                         *
222900960607     C*---------------------------------------------------------------*
223000960607     C     RIPRIS        BEGSR
223100130509     C*
223200130509     c                   eval      Operazione = Ripristina
223300130509     c                   exsr      AGGTabella
223400960607     C*
223500960607     C                   ENDSR
223600130509     C*---------------------------------------------------------------*
223700130509     C*  AGGTAB: Ripristina o Annulla  la Tabella                     *
223800130509     C*---------------------------------------------------------------*
223900130509     C     AGGTABella    BEGSR
224000130509      *
224100130509     C                   MOVEL     *BLANKS       KKEY
224200130509     C                   MOVEL     V2CKEY        KKEY
224300130509     c                   z-add     0             conta_tab         3 0
224400130509      *
224500130509     c                   do        *hival
224600130509     c                   eval        conta_tab = conta_tab + 1
224700130509      *
224800130509     c                   if          conta_tab = 1
224900130509     C                   MOVEL     'PT'          KCOD
225000130509     c                   elseif      conta_tab = 2
225100130509     C                   MOVEL     'PU'          KCOD
225200130509     c                   elseif      conta_tab = 3
225300130509     C                   MOVEL     'PV'          KCOD
225400130509     c                   elseif      conta_tab = 4
225500130509     C                   MOVEL     'PW'          KCOD
225600130509     c                   elseif      conta_tab = 5
225700130509     C                   MOVEL     'PZ'          KCOD
225800130509     c                   elseif      conta_tab = 6
225900130509     C                   MOVEL     'P1'          KCOD
226000130509     c                   elseif      conta_tab = 7
226100130509     C                   MOVEL     'P2'          KCOD
226200130509     c                   elseif      conta_tab = 8
226300130509     C                   MOVEL     'P3'          KCOD
226400130509     c                   elseif      conta_tab = 9
226500130509     C                   MOVEL     'P4'          KCOD
226600130509     c                   elseif      conta_tab = 10
226700130509     C                   MOVEL     'P5'          KCOD
226800130509     c                   elseif      conta_tab = 11
226900130509     C                   MOVEL     'PO'          KCOD
227000140221     c                   elseif      conta_tab = 12
227100140221     C                   MOVEL     'PS'          KCOD
227200130509     c                   else
227300130509      *
227400130509     c                   leave
227500130509      *
227600130509     c                   end
227700130509      *
227800130509     C     KTAB          CHAIN     EDTAB01L
227900130509     C                   IF        %Found(EDTAB01L)
228000130509      **  RIPRISTINA
228100130509     c                   if        Operazione = Ripristina
228200130509     C                   MOVEL     ' '           TABFLG
228300130509     C                   MOVEL     ' '           TABFTR
228400130509     C                   Z-ADD     0             TABDTR
228500130509      **  ANNULLA
228600130509     c                   elseif    Operazione = Annulla
228700130509     C                   MOVEL     '*'           TABFLG
228800130509     C                   clear                   TABFTR
228900130509     C                   clear                   TABDTR
229000130509      **
229100130509     c                   end
229200130509      **
229300130509     C                   EXCEPT    TABANN
229400130509     C                   END
229500130509      *
229600130509     C                   ENDdo
229700130509      *
229800130509     C                   MOVEL     '1'           $GESD
229900130509     C*
230000130509     C                   ENDSR
230100130509     C*---------------------------------------------------------------*
230200960607     C*  CONFER: Confermo inserimento/variazione                      *
230300960607     C*---------------------------------------------------------------*
230400960607     C     CONFER        BEGSR
230500960607     C*
230600130509     C                   MOVEL     '1'           $GESD
230700130520     c***************    goto      endCONFER
230800110913     C*
230900960607     C*  Gestisco scrittura/aggiornamento key
231000960607     C                   MOVEL     *BLANKS       KKEY
231100960611     C                   MOVEL     V2CKEY        KKEY
231200960611     C                   MOVEL     'PT'          KCOD
231300960607     C     KTAB          CHAIN     EDTAB01L                           30
231400961021     C                   Z-ADD     V2CKSC        §PTKSC
231500960611     C                   Z-ADD     V2CLNP        §PTLNP
231600960611     C                   Z-ADD     V2CNRS        §PTNRS
231700960611     C                   MOVEL     V2CNAZ        §PTNAZ
231800960611     C                   MOVEL     V2CCTM        §PTCTM
231900960611     C                   MOVEL     V2CCTR        §PTCTR
232000960827     C                   MOVEL     V2CMSQ        §PTMSQ
232100960611     C                   MOVEL     V2CPLT        §PTPLT
232200960627     C                   MOVEL     V2CLOC        §PTLOC
232300960827     C                   MOVEL     V3CCMR        §PTCMR
232400130529      * il primo flag a sinistra del campo xxx
232500130529     C                   MOVEL     V3Cias        §PTxxx
232600960827     C                   MOVEL     V3CNFV        §PTNFV
232700960827     C                   MOVEL     V3CDES        §PTDES
232800960911     C                   MOVEL     V6CCM1        §PTCM1
232900960911     C                   MOVEL     V6CNF1        §PTNF1
233000960911     C                   MOVEL     V6CDET        §PTDET
233100960827     C                   MOVEL     V4CCM2        §PTCM2
233200960827     C                   MOVEL     V4CNF2        §PTNF2
233300960827     C                   MOVEL     V4CLO1        §PTLC1
233400960827     C                   MOVEL     V4CLO2        §PTLC2
233500960827     C                   MOVEL     V4CDT1        §PTDT1
233600960827     C                   MOVEL     V4CEQD        §PTEQD
233700960827     C                   MOVEL     V4CCN1        §PTCN1
233800960827     C                   MOVEL     V4CCN2        §PTCN2
233900960827     C                   MOVEL     V4CCN3        §PTCN3
234000960827     C                   MOVEL     V4CCN4        §PTCN4
234100960827     C                   MOVEL     V4CFTT        §PTFTT
234200120117     C********           MOVEL     V4CNSP        §PTNSP
234300960905     C                   MOVEL     V4CAGE        §PTAGE
234400100209     C                   MOVEL     V6Ctrd        §PTtrd
234500100722     C                   MOVEL     V6Crmn        §PTsgn
234600960906     C                   MOVEL     DSPT          TABUNI
234700960827     C                   MOVEL     V5CFTT        TABFTT
234800960827     C                   Z-ADD     V5CFLT        TABFLT
234900960827     C     V5CFTT        IFNE      ' '
235000960607     C                   MOVEL     ' '           TABFTR
235100960607     C                   Z-ADD     0             TABDTR
235200960607     C                   END
235300960607     C     *IN30         IFEQ      '0'
235400960607     C                   UPDATE    EDTAB000
235500960607     C                   ELSE
235600960611     C                   MOVEL     V2CKEY        TABKEY
235700960611     C                   MOVEL     'PT'          TABCOD
235800960607     C                   WRITE     EDTAB000
235900960607     C                   END
236000960905     C*  Aggiorno PU
236100960905     C                   MOVEL     'PU'          KCOD
236200960905     C     KTAB          CHAIN     EDTAB01L                           30
236300961021     C                   MOVEL     V2CTPC        §PUTPC
236400080623      * Orm Automatici: Cliente/Tariffa
236500080623     C                   move      V2Oksc        §puORMaKSC
236600080623     C                   eval      §puORMaCTR = V2Octr
236700961021     C                   MOVEL     V6CLAB        §PULAB
236800101028     C                   MOVEL     V6CFIRMA      §PUFIRMA
236900960906     C                   MOVEL     V4CNSA        §PUNSA
237000960906     C                   MOVEL     V4CSEC        §PUSEC
237100010730     C                   MOVEL     V4CVOL        §PUVOL
237200050525     C                   MOVEL     V4Vman        §PUVma
237300060912     C                   MOVEL     V4cISC        §PUisc
237400961126     C                   MOVEL     V4CSPC        §PUSPC
237500100407     C                   MOVEL     V4ftxAAA      §PUftxAAA
237600110525     C                   MOVEL     V2Compat      §PUcompat
237700980320     C                   MOVEL     V2CBS1        §PUBS1
237800050831     C                   MOVEL     V2Csig        §PUsig
237900110628     C                   MOVEL     V2Cnomfile    §PUnomfile
238000111017     C                   MOVEL     V3Cres        §PUres
238100070215     C                   MOVEL     V2Cpln        §PUpln
238200960911     C                   Z-ADD     V3CEL1        §PUEL1
238300960911     C                   Z-ADD     V3CLP1        §PULP1
238400960911     C                   Z-ADD     V3CLA1        §PULA1
238500960911     C                   Z-ADD     V3CNR1        §PUNR1
238600960911     C                   Z-ADD     V3CSG1        §PUSG1
238700960911     C                   Z-ADD     V3CZN1        §PUZN1
238800960923     C                   MOVEL     V3CWRK        §PUWRK
238900960925     C                   MOVEL     V3CLNA        §PULNA
239000120402     C                   MOVEL     VPTxxx        §PUxxx
239100100921     C                   MOVEL     V3ragDES      §PURAGGDES
239200100921     C                   MOVEL     V3nomCMR      §PUNOMECMR
239300100921     C                   MOVEL     V3inGIO       §PUINGIO
239400960926     C                   MOVEL     V3CCAS        §PUCAS
239500050223     C                   MOVEL     V3Cnsp        §PUnsp
239600050301     C                   move      V3Clun        §PUlun
239700050520     C                   move      V3Cnas        §PUnas
239800030708     C                   MOVEL     V4CGGV        §PUGGV
239900961021     C                   MOVEL     V6CRFF        §PURFF
240000960911     C                   Z-ADD     V6CEL2        §PUEL2
240100960911     C                   Z-ADD     V6CLP2        §PULP2
240200960911     C                   Z-ADD     V6CLA2        §PULA2
240300960911     C                   Z-ADD     V6CNR2        §PUNR2
240400960911     C                   Z-ADD     V6CSG2        §PUSG2
240500960911     C                   Z-ADD     V6CZN2        §PUZN2
240600100512     C                   movel     V6C203        §PUdate203
240700961021     C                   MOVEL     V7CDTA        §PUDTA
240800961021     C                   MOVEL     V7CDTB        §PUDTB
240900961021     C                   MOVEL     V7CSGD        §PUSGD
241000961021     C                   MOVEL     V7CECC        §PUECC
241100010802     C                   MOVEL     V7CATR        §PUATR
241200061127     C                   MOVEL     V3Css         §PUss
241300061127     C                   MOVEL     V3Ctb         §PUtb
241400061127     C                   MOVEL     V3Ctc         §PUtc
241500061127     C                   MOVEL     V3Cts         §PUts
241600061128     C                   MOVEL     V3Cgr         §PUgr
241700070221     C                   MOVEL     V3Cpfa        §PUpfa
241800140221      *----
241900960906     C                   MOVEL     *BLANKS       TABUNI
242000960906     C                   MOVEL     DSPU          TABUNI
242100960905     C                   MOVEL     V5CFTT        TABFTT
242200960905     C                   Z-ADD     V5CFLT        TABFLT
242300960905     C     V5CFTT        IFNE      ' '
242400960905     C                   MOVEL     ' '           TABFTR
242500960905     C                   Z-ADD     0             TABDTR
242600960905     C                   END
242700960905     C     *IN30         IFEQ      '0'
242800960905     C                   UPDATE    EDTAB000
242900960905     C                   ELSE
243000960905     C                   MOVEL     V2CKEY        TABKEY
243100960905     C                   MOVEL     'PU'          TABCOD
243200960905     C                   WRITE     EDTAB000
243300960905     C                   END
243400070123     C*  Aggiorno PV
243500070123     C                   MOVEL     'PV'          KCOD
243600070123     C     KTAB          CHAIN     EDTAB01L                           30
243700070202     C                   MOVEL     V8EMAI        §PVEMA
243800070131     C                   MOVEL     V8CRFF        §PVRFF
243900070131     C                   MOVEL     V8CRFS        §PVRFS
244000070201     C                   MOVEL     V8CRSE        §PVRSE
244100070131     C                   MOVEL     V8CRCU        §PVRCU
244200070205     C                   MOVEL     V8Cpes        §PVpes
244300070205     C                   MOVEL     V8Ccol        §PVcol
244400070206     C                   MOVEL     V8Csin        §PVsin
244500070206     C                   MOVEL     V8Cicn        §PVicn
244600070206     C                   MOVEL     V8C2in        §PV2in
244700070219     C                   MOVEL     V8Cinv        §PVinv
244800071128     C                   MOVEL     V8Ccni        §PVcniRMN
244900111221     C                   MOVEL     V7CCNVSTS     §PVCNVSTS
245000111221     C                   MOVEL     V7CIDEnt      §PVIDENT
245100120330     C                   MOVEL     V4quaPCI      §PVquaPCI
245200120330     C                   MOVEL     V4xSEG        §PVpciXseg
245300120118      *----
245400120118     C                   eval      §PVCURESO = V4CURESO
245500120118     C                   eval      §PVCUQUAR = V4CUQUR
245600120118     C                   eval      §PVCUTIPR = V4CUTIPR
245700120118     C                   eval      §PVCUORM  = V4CUORM
245800120118     C                   eval      §PVCUQUAO = V4CUQUO
245900120118     C                   eval      §PVCUTIPO = V4CUTIPO
246000120118      *----
246100070123     C                   MOVEL     *BLANKS       TABUNI
246200070123     C                   MOVEL     DSPV          TABUNI
246300070123     C                   MOVEL     V5CFTT        TABFTT
246400070123     C                   Z-ADD     V5CFLT        TABFLT
246500070123     C     V5CFTT        IFNE      ' '
246600070123     C                   MOVEL     ' '           TABFTR
246700070123     C                   Z-ADD     0             TABDTR
246800070123     C                   END
246900070123     C     *IN30         IFEQ      '0'
247000070123     C                   UPDATE    EDTAB000
247100070123     C                   ELSE
247200070123     C                   MOVEL     V2CKEY        TABKEY
247300070123     C                   MOVEL     'PV'          TABCOD
247400070123     C                   WRITE     EDTAB000
247500070123     C                   END
247600101001     C*
247700101019     C*  Aggiorno PW
247800101019     C                   MOVEL     'PW'          KCOD
247900101019     C     KTAB          CHAIN     EDTAB01L                           30
248000101019     C                   MOVEL     VPTNCOM       §PWCOMMER
248100101019     C                   MOVEL     VPTNMAIL      §PWCONTACT
248200101019     C                   MOVEL     *BLANKS       §PWNAZIONE
248300140221      *----
248400101019     C                   MOVEL     *BLANKS       TABUNI
248500101019     C                   MOVEL     DSPW          TABUNI
248600101019     C                   MOVEL     V5CFTT        TABFTT
248700101019     C                   Z-ADD     V5CFLT        TABFLT
248800101019     C     V5CFTT        IFNE      ' '
248900101019     C                   MOVEL     ' '           TABFTR
249000101019     C                   Z-ADD     0             TABDTR
249100101019     C                   END
249200101019     C     *IN30         IFEQ      '0'
249300101019     C                   UPDATE    EDTAB000
249400101019     C                   ELSE
249500101019     C                   MOVEL     V2CKEY        TABKEY
249600101019     C                   MOVEL     'PW'          TABCOD
249700101019     C                   WRITE     EDTAB000
249800101019     C                   END
249900101019     C*
250000101019     C*  Aggiorno PZ
250100101019     C                   MOVEL     'PZ'          KCOD
250200101019     C     KTAB          CHAIN     EDTAB01L                           30
250300101019     C                   MOVEL     VRCZTB        §PZTIPRCZB
250400101019     C                   MOVEL     VRCZFB        §PZFILRCZB
250500101019     C                   MOVEL     VRCZPB        §PZPGMRCZB
250600101019     C                   MOVEL     VTRSTB        §PZTIPTRSB
250700101019     C                   MOVEL     VTRSFB        §PZFILTRSB
250800101019     C                   MOVEL     VTRSPB        §PZPGMTRSB
250900101019     C                   MOVEL     VRCZTS        §PZTIPRCZS
251000101019     C                   MOVEL     VRCZFS        §PZFILRCZS
251100101019     C                   MOVEL     VRCZPS        §PZPGMRCZS
251200101019     C                   MOVEL     VTRSTS        §PZTIPTRSS
251300101019     C                   MOVEL     VTRSFS        §PZFILTRSS
251400101019     C                   MOVEL     VTRSPS        §PZPGMTRSS
251500110705     C                   MOVEL     VPTCONNEX     §PZFTPCOD
251600120206     C                   MOVEL     V2cexp        §PZkscEXP
251700120628     C                   MOVEL     V3nadSF       §PZnadSF
251800120705     C                   MOVEL     V4CGWC        §PZtsrGWC
251900130215     C                   MOVEL     V4sufTB       §PZsuffTAB
252000140221      *----
252100101019     C                   MOVEL     *BLANKS       TABUNI
252200101019     C                   MOVEL     DSPZ          TABUNI
252300101019     C                   MOVEL     V5CFTT        TABFTT
252400101019     C                   Z-ADD     V5CFLT        TABFLT
252500101019     C     V5CFTT        IFNE      ' '
252600101019     C                   MOVEL     ' '           TABFTR
252700101019     C                   Z-ADD     0             TABDTR
252800101019     C                   END
252900101019     C     *IN30         IFEQ      '0'
253000101019     C                   UPDATE    EDTAB000
253100101019     C                   ELSE
253200101019     C                   MOVEL     V2CKEY        TABKEY
253300101019     C                   MOVEL     'PZ'          TABCOD
253400101019     C                   WRITE     EDTAB000
253500101019     C                   END
253600130412     C*
253700140221     C*  Aggiorno PS
253800140221     C                   MOVEL     'PS'          KCOD
253900140221     C     KTAB          CHAIN     EDTAB01L                           30
254000140221      *
254100161013     C                   MOVEL     V6NADCN       §PSNADCN
254200161013     C                   MOVEL     V6cnsu21      §PScnsu21
254300140221     C                   MOVEL     V6CAPP        §PSAPP
254400140416     C                   MOVEL     V6Crcu        §PSrcu
254500160706     C                   eval      §PSftxnad = V6ftxnad
254600160209     c                   if        V2fgs = 0
254700160209     C                   clear                   §PSfgs
254800160209     c                   else
254900160205     C                   MOVEL     V2fgs         §PSfgs
255000160209     c                   end
255100140221      *
255200140221     C                   MOVEL     *BLANKS       TABUNI
255300140221     C                   MOVEL     DSPS          TABUNI
255400140221     C                   MOVEL     V5CFTT        TABFTT
255500140221     C                   Z-ADD     V5CFLT        TABFLT
255600140221     C     V5CFTT        IFNE      ' '
255700140221     C                   MOVEL     ' '           TABFTR
255800140221     C                   Z-ADD     0             TABDTR
255900140221     C                   END
256000140221     C     *IN30         IFEQ      '0'
256100140221     C                   UPDATE    EDTAB000
256200140221     C                   ELSE
256300140221     C                   MOVEL     V2CKEY        TABKEY
256400140221     C                   MOVEL     'PS'          TABCOD
256500140221     C                   WRITE     EDTAB000
256600140221     C                   END
256700140221     C*
256800130412     C*  Aggiorno PO
256900130412     C                   MOVEL     'PO'          KCOD
257000130412     C     KTAB          CHAIN     EDTAB01L                           30
257100130412      *
257200130412     C                   MOVEL     VRCZTO        §POTIPRCZo
257300130412     C                   MOVEL     VRCZFO        §POFILRCZo
257400130412     C                   MOVEL     VRCZPO        §POPGMRCZo
257500130412     C                   MOVEL     VTRSTO        §POTIPTRSo
257600130412     C                   MOVEL     VTRSFO        §POFILTRSo
257700130412     C                   MOVEL     VTRSPO        §POPGMTRSo
257800130522     C                   MOVEL     VPOsndPRTF    §PoSNDprtf
257900130412      *
258000130412     C                   MOVEL     *blank        §POTIPRCZs
258100130412     C                   MOVEL     *blank        §POFILRCZs
258200130412     C                   MOVEL     *blank        §POPGMRCZs
258300130412     C                   MOVEL     *blank        §POTIPTRSs
258400130412     C                   MOVEL     *blank        §POFILTRSs
258500130412     C                   MOVEL     *blank        §POPGMTRSs
258600130412      *
258700130412     C                   MOVEL     *BLANKS       TABUNI
258800130412     C                   MOVEL     DSPO          TABUNI
258900130412     C                   MOVEL     V5CFTT        TABFTT
259000130412     C                   Z-ADD     V5CFLT        TABFLT
259100130412     C     V5CFTT        IFNE      ' '
259200130412     C                   MOVEL     ' '           TABFTR
259300130412     C                   Z-ADD     0             TABDTR
259400130412     C                   END
259500130412     C     *IN30         IFEQ      '0'
259600130412     C                   UPDATE    EDTAB000
259700130412     C                   ELSE
259800130412     C                   MOVEL     V2CKEY        TABKEY
259900130412     C                   MOVEL     'PO'          TABCOD
260000130412     C                   WRITE     EDTAB000
260100130412     C                   END
260200110819     C*
260300110913     C*  Aggiorno P1-2-3-4-5  Note (PN)
260400110913     C                   MOVEL     'P1'          KCOD
260500110913     C     KTAB          chain     EDTAB01L
260600110913      ** sia in Write che Update
260700110913     C                   MOVEL     V5CFTT        TABFTT
260800110913     C                   Z-ADD     V5CFLT        TABFLT
260900110913     C     V5CFTT        IFNE      ' '
261000110913     C                   MOVEL     ' '           TABFTR
261100110913     C                   Z-ADD     0             TABDTR
261200110913     C                   END
261300110913     c                   clear                   dsPN
261400110913     C                   eval       §Pnriga  = 1
261500110913     C                   eval       §PnNOTA  = not001
261600110913      *** non ci sono Note
261700110913     c                   if        not %Found(EDTAB01L)
261800110913     C                   MOVEL     V2CKEY        TABKEY
261900110913     C                   MOVEL     'P1'          TABCOD
262000110914     c                   z-add     data8         §PnDTINI
262100110913     c                   eval      tabUNI = dsPN
262200110913     C                   WRITE     EDTAB000
262300110913     c                   else
262400110914     c                   z-add     v2dadat       §PnDTINI
262500110913     C                   eval       tabUNI = DSPN
262600110913     C                   UPDATE    EDTAB000
262700110913     c                   end
262800110913      ***
262900110913     C                   MOVEL     'P2'          KCOD
263000110913     C     KTAB          chain     EDTAB01L
263100110913      ** sia in Write che Update
263200110913     C                   MOVEL     V5CFTT        TABFTT
263300110913     C                   Z-ADD     V5CFLT        TABFLT
263400110913     C     V5CFTT        IFNE      ' '
263500110913     C                   MOVEL     ' '           TABFTR
263600110913     C                   Z-ADD     0             TABDTR
263700110913     C                   END
263800110913     c                   clear                   dsPN
263900110913     C                   eval       §Pnriga  = 2
264000110913     C                   eval       §PnNOTA  = not002
264100110913      *** non ci sono Note
264200110913     c                   if        not %Found(EDTAB01L)
264300110913     C                   MOVEL     V2CKEY        TABKEY
264400110913     C                   MOVEL     'P2'          TABCOD
264500110914     c                   z-add     data8         §PnDTINI
264600110913     c                   eval      tabUNI = dsPN
264700110913     C                   WRITE     EDTAB000
264800110913     c                   else
264900110914     c                   z-add     v2dadat       §PnDTINI
265000110913     C                   eval       tabUNI = DSPN
265100110913     C                   UPDATE    EDTAB000
265200110913     c                   end
265300110913      ***
265400110913     C                   MOVEL     'P3'          KCOD
265500110913     C     KTAB          chain     EDTAB01L
265600110913      ** sia in Write che Update
265700110913     C                   MOVEL     V5CFTT        TABFTT
265800110913     C                   Z-ADD     V5CFLT        TABFLT
265900110913     C     V5CFTT        IFNE      ' '
266000110913     C                   MOVEL     ' '           TABFTR
266100110913     C                   Z-ADD     0             TABDTR
266200110913     C                   END
266300110913     c                   clear                   dsPN
266400110913     C                   eval       §Pnriga  = 3
266500110913     C                   eval       §PnNOTA  = not003
266600110913      *** non ci sono Note
266700110913     c                   if        not %Found(EDTAB01L)
266800110913     C                   MOVEL     V2CKEY        TABKEY
266900110913     C                   MOVEL     'P3'          TABCOD
267000110914     c                   z-add     data8         §PnDTINI
267100110913     c                   eval      tabUNI = dsPN
267200110913     C                   WRITE     EDTAB000
267300110913     c                   else
267400110914     c                   z-add     v2dadat       §PnDTINI
267500110913     C                   eval       tabUNI = DSPN
267600110913     C                   UPDATE    EDTAB000
267700110913     c                   end
267800110913      ***
267900110913     C                   MOVEL     'P4'          KCOD
268000110913     C     KTAB          chain     EDTAB01L
268100110913      ** sia in Write che Update
268200110913     C                   MOVEL     V5CFTT        TABFTT
268300110913     C                   Z-ADD     V5CFLT        TABFLT
268400110913     C     V5CFTT        IFNE      ' '
268500110913     C                   MOVEL     ' '           TABFTR
268600110913     C                   Z-ADD     0             TABDTR
268700110913     C                   END
268800110913     c                   clear                   dsPN
268900110913     C                   eval       §Pnriga  = 4
269000110913     C                   eval       §PnNOTA  = not004
269100110913      *** non ci sono Note
269200110913     c                   if        not %Found(EDTAB01L)
269300110913     C                   MOVEL     V2CKEY        TABKEY
269400110913     C                   MOVEL     'P4'          TABCOD
269500110914     c                   z-add     data8         §PnDTINI
269600110913     c                   eval      tabUNI = dsPN
269700110913     C                   WRITE     EDTAB000
269800110913     c                   else
269900110914     c                   z-add     v2dadat       §PnDTINI
270000110913     C                   eval       tabUNI = DSPN
270100110913     C                   UPDATE    EDTAB000
270200110913     c                   end
270300110913      ***
270400110913     C                   MOVEL     'P5'          KCOD
270500110913     C     KTAB          chain     EDTAB01L
270600110913      ** sia in Write che Update
270700110913     C                   MOVEL     V5CFTT        TABFTT
270800110913     C                   Z-ADD     V5CFLT        TABFLT
270900110913     C     V5CFTT        IFNE      ' '
271000110913     C                   MOVEL     ' '           TABFTR
271100110913     C                   Z-ADD     0             TABDTR
271200110913     C                   END
271300110913     c                   clear                   dsPN
271400110913     C                   eval       §Pnriga  = 5
271500110913     C                   eval       §PnNOTA  = not005
271600110913      *** non ci sono Note
271700110913     c                   if        not %Found(EDTAB01L)
271800110913     C                   MOVEL     V2CKEY        TABKEY
271900110913     C                   MOVEL     'P5'          TABCOD
272000110914     c                   z-add     data8         §PnDTINI
272100110913     c                   eval      tabUNI = dsPN
272200110913     C                   WRITE     EDTAB000
272300110913     c                   else
272400110914     c                   z-add     v2dadat       §PnDTINI
272500110913     C                   eval       tabUNI = DSPN
272600110913     C                   UPDATE    EDTAB000
272700110913     c                   end
272800110913      ***
272900110913     C*
273000110819     C*   Solo se Partner occorre allineare anche un'altra tabella x EEX
273100110819     c                   if        V2CTPC = 'P'
273200110819     c                   exsr      se_Partner
273300110819     c                   end
273400070215     C*
273500130520     C     endCONFER     ENDSR
273600130515     C*---------------------------------------------------------------*
273700130515     C*  se è un Partner EEX occorre tenere allineata la tabella
273800130515     C*  correlata con decorrenze e scadenze dei codici in vigore
273900130515     C*---------------------------------------------------------------*
274000130515     C     se_Partner    BEGSR
274100130515     C*
274200130515     C                   EXFMT     TC93W03
274300130515      **
274400130515     c                   close     TRTC93D
274500130515     C                   MOVEL     KPJBU         KPJBUs
274600130515???? c                   call      'TRTC70R'                            90
274700130515???? c                   parm                    KPJBA
274800130515     C                   MOVEL     KPJBUs        KPJBU
274900130515     c                   open      TRTC93D
275000130515     C*
275100130515     C                   endSR
275200070215     C*---------------------------------------------------------------*
275300110913     C*
275400110913     C*---------------------------------------------------------------*
275500110913     c     WinNOTE       BegSR
275600110913     C*
275700110913     C                   exfmt     tc93NOT
275800110913     c     NOT001        comp      *blank                             0202
275900110913     c  n02NOT002        comp      *blank                             0202
276000110913     c  n02NOT003        comp      *blank                             0202
276100110913     c  n02NOT004        comp      *blank                             0202
276200110913     c  n02NOT005        comp      *blank                             0202
276300110913     C*
276400110913     C                   EndSR
276500110913     C*---------------------------------------------------------------*
276600070215     C*  *INZSR: Operazioni iniziali                                  *
276700070215     C*---------------------------------------------------------------*
276800960606     C     *INZSR        BEGSR
276900960606     C*
277000960606     C     *ENTRY        PLIST
277100960606     C                   PARM                    KPJBA
277200130118     c*
277300130118     c                   open      TRTC93D
277400960606      *  Call a XPARUT
277500960606     C                   Z-ADD     1             CODUT
277600960606     C                   CALL      'X§PARUT'
277700960606     C                   PARM                    UT§DSE
277800960606     C                   MOVEL     REC80         CNCR80
277900960611     C*--- RICERCA CAPOCONTI------------------------------------------*
278000960611     C                   DO        50            X
278100960611     C                   MOVE      TCU(X)        TCUDS
278200960611     C     F56           IFEQ      'CG'
278300960611     C     F34           ANDEQ     '01'
278400960611     C                   Z-ADD     KCU(X)        KCI               4 0
278500960611     C                   END
278600960611     C                   END
278700960606     C                   MOVEL     RAGUT         RSUT
278800960606      * Definizione chiavi
278900960606     C     KTAB          KLIST
279000960606     C                   KFLD                    KCOD
279100960606     C                   KFLD                    KKEY
279200960611     C     KACO          KLIST
279300960611     C                   KFLD                    KKUT
279400960611     C                   KFLD                    KKCC
279500960611     C                   KFLD                    KKSC
279600960611     C     KTAB1         KLIST
279700960611     C                   KFLD                    KKUT
279800960611     C                   KFLD                    KCOD1
279900120416     C     Ktabel        KLIST
280000120416     C                   KFLD                    tblkut
280100120416     C                   KFLD                    tblcod
280200120416     C                   KFLD                    tblkey
280300960606      * Definizione varibili
280400960606     C     *LIKE         DEFINE    TABCOD        KCOD
280500960606     C     *LIKE         DEFINE    TABKEY        KKEY
280600960606     C     *LIKE         DEFINE    TABKEY        WINKEY
280700960611     C     *LIKE         DEFINE    ACOKUT        KKUT
280800960611     C     *LIKE         DEFINE    ACOKCC        KKCC
280900960611     C     *LIKE         DEFINE    ACOKSC        KKSC
281000960611     C     *LIKE         DEFINE    TBLCOD        KCOD1
281100960611     C     *LIKE         DEFINE    TBLKUT        §KUT
281200960611     C     *LIKE         DEFINE    TBLCOD        §COD
281300960611     C     *LIKE         DEFINE    TBLKEY        §KEY
281400960611     C     *LIKE         DEFINE    ACOKCC        PARKCC
281500960611     C     *LIKE         DEFINE    ACOKSC        PARKSC
281600960606     C* Inizializzo variabili
281700960606     C                   MOVEL     'N'           WFINE             1
281800960611     C* Caricamento dati tabella 1b
281900131010     C                   Z-ADD     0             X                 3 0
282000960611     C                   Z-ADD     1             KKUT
282100960611     C                   MOVEL     '1B'          KCOD1
282200960611     C     KTAB1         CHAIN     TABEL00F                           30
282300960611     C     *IN30         DOWEQ     '0'
282400960611     C     TBLFLG        IFEQ      *BLANKS
282500960611     C                   ADD       1             X
282600960611     C                   MOVEL     TBLKEY        C1B(X)
282700960611     C                   MOVEL     TBLUNI        D1B(X)
282800960611     C                   END
282900960611     C     KTAB1         READE     TABEL00F                               30
283000960611     C                   END
283100960611     C* Caricamento dati tabella 15
283200131010     C                   Z-ADD     0             X                 3 0
283300960611     C                   Z-ADD     1             KKUT
283400960611     C                   MOVEL     '15'          KCOD1
283500960611     C     KTAB1         CHAIN     TABEL00F                           30
283600960611     C     *IN30         DOWEQ     '0'
283700960611     C     TBLFLG        IFEQ      *BLANKS
283800960611     C                   ADD       1             X
283900960611     C                   MOVEL     TBLKEY        C15(X)
284000960611     C                   MOVEL     TBLUNI        D15(X)
284100960611     C                   END
284200960611     C     KTAB1         READE     TABEL00F                               30
284300960611     C                   END
284400960606     C*
284500051209     c                   move      'PU'          tabPU             2
284600051209     c                   z-add     0             NSig
284700051209     c     tabPU         setll     edtab01l
284800051209     c     tabPU         reade     edtab01l
284900051209     c                   dow       not %EoF(edtab01l)
285000051209     c                   movel     tabuni        dsPU
285100051209     c                   add       1             NSig
285200051209     C                   movel     §puSIG        Psig(Nsig)
285300051209     C                   movel     tabkey        Csig(Nsig)
285400051209     c     tabPU         reade     edtab01l
285500051209     c                   enddo
285600051209     C*
285700070215     C* carica tutte le linee gestite x controllo su duplica UNB
285800070215     c                   move      'PT'          tabPT             2
285900070215     c                   z-add     0             Nlnp
286000070215     c     tabPT         setll     edtab01l
286100070215     c     tabPT         reade     edtab01l
286200070215     c                   dow       not %EoF(edtab01l)
286300070215     c                   movel     tabuni        dsPT
286400070215     c                   add       1             Nlnp
286500070215     C                   z-add     §ptLNP        PTlnp(Nlnp)
286600070215     c     tabPT         reade     edtab01l
286700070215     c                   enddo
286800070215     C*
286900110608     c                   clear                   PR_DS
287000110608     c                   clear                   PR_key
287100110608     c                   clear                   PI_DS
287200110608     c                   clear                   PI_key
287300110608     c                   clear                   PF_DS
287400110608     c                   clear                   PF_key
287500101020     C*
287600101020     C* carica tutti programmi gestiti di traduzione
287700101020     c                   z-add     0             Npr               3 0
287800101020     c                   move      'PR'          tabPR             2
287900101020     c     tabPR         setll     edtab01l
288000101020     c     tabPR         reade     edtab01l
288100101020     c                   dow       not %EoF(edtab01l)
288200101020     c                   movel     tabuni        dsPR
288300101020     c                   add       1             Npr
288400101020     C                   movel     dsPR          PR_DS(Npr)
288500101020     C                   movel     TABKEY        PR_key(Npr)
288600101020     c     tabPR         reade     edtab01l
288700101020     c                   enddo
288800101020     C*
288900101020     c                   z-add     0             Npr
289000101020     c                   move      'PF'          tabPF             2
289100101020     c     tabPF         setll     edtab01l
289200101020     c     tabPF         reade     edtab01l
289300101020     c                   dow       not %EoF(edtab01l)
289400101020     c                   movel     tabuni        dsPF
289500101020     c                   add       1             Npr
289600101020     C                   movel     §PFDEFIN      PF_DS(Npr)
289700101020     C                   movel     TABKEY        PF_key(Npr)
289800101020     c     tabPF         reade     edtab01l
289900101020     c                   enddo
290000101020     C*
290100101020     c                   z-add     0             Npr
290200101020     c                   move      'PI'          tabPI             2
290300101020     c     tabPI         setll     edtab01l
290400101020     c     tabPI         reade     edtab01l
290500101020     c                   dow       not %EoF(edtab01l)
290600101020     c                   movel     tabuni        dsPI
290700101020     c                   add       1             Npr
290800101020     C                   movel     dsPI          PI_DS(Npr)
290900101020     C                   movel     TABKEY        PI_key(Npr)
291000101020     c     tabPI         reade     edtab01l
291100101020     c                   enddo
291200101020     C*
291300110914     C                   time                    datatime         14 0
291400110914     C                   move      datatime      data8             8 0
291500110914     C*
291600960606     C                   ENDSR
291700000307      *-----------------------------------------------------*
291800111221     C*
291900111221      *-----------------------------------------------------*
292000111221     C     OPEN_SR1      BEGSR
292100111221      *
292200111221     C                   MOVEL     KPJBU         KPJBUs
292300111221      *
292400111221     c                   if        v7cIDEnt = *blank
292500111221     c                   eval      XTAUNBcli = V2CKEY
292600111221     c                   else
292700111221     c                   eval      XTAUNBcli = V7Cident
292800111221     c                   end
292900111221      *
293000111221     c                   eval      XTARAGs   = v2dksc
293100111221      *
293200111221     C                   eval      KPJBU = TABDS
293300111221     c                   call      'TRTC93SR1'
293400111221     c                   parm                    KPJBA
293500111221     C                   eval      TABDS = KPJBU
293600111221      *
293700111221     C                   MOVEL     KPJBUs        KPJBU
293800111221      *
293900111221     C                   ENDSR
294000111221      *-----------------------------------------------------*
294100960607     OEDTAB000  E            TABANN
294200960607     O                       TABFLG
294300960607     O                       TABFLT
294400960607     O                       TABFTT
294500000307     O                       TABFTR
294600000307     O                       TABDTR
294700000307      *-----------------------------------------------------*
294800961011**         ERR
294900960611Codice Partner obbligatorio                                            01
295000960611Codice Partner inesistente                                             02
295100960612Codice Partner annullato                                               03
295200960611Linea di partenza obbligatoria                                         04
295300960611Linea di partenza inesistente                                          05
295400960611Linea di partenza annullata                                            06
295500960627Nazione obbligatoria                                                   07
295600960627Nazione inesistente                                                    08
295700960627Codice Platform list obbligatorio                                      09
295800960627Codice Trattamento merce obbligatorio                                  10
295900960627Codice Trattamento merce inesisitente                                  11
296000960627Codice Trattamento merce richiede il numero di serie                   12
296100960627Codice Tariffa obbligaotorio                                           13
296200960627Luogo di partenza/arrivo obbligatorio                                  14
296300960627Immettere filiale a cui trasmettere                                    15
296400960627Se trasmissione a tutti o a nessuno impossibile immettere filiale      16
296500960627Filale a cui tramettere inesistente                                    17
296600960827Se devo ritornare numero CMR dovrò riceverlo                           18
296700960827Se devo ritornare numero Foglio viaggio dovrò riceverlo                19
296800971204Se non mi passano ns.numero segnacollo non devo impostare dati sgn.    20
296900960911Se mi passano segnacollo indicare anche altri dati                     21
297000971204Se devo ritornare ns.numero segnacollo dovrò riceverlo                 22
297100140416Se NON devo inviare nostro nr.segnacollo NON impostare DATI segnac.    23
297200140416Se devo ritornare segnacollo indicare TUTTI I DATI NECESSARI           24
297300961011Impossible indicare ritorno eccedenze si se non invio le date arrivo   25
297400961011Trasmissione bolle impossibile se non previsto invio date di arrivo    26
297500961011Trasmissione segnacolli impossibile se non invio le date di arrivo     27
297600050831Sigla   obbligatoria                                                   28
297700100210Sigla già esistente su altro codice Cliente/Partner -> F8 x Forzare    29
297800051213Impostare il nr.di segnacolli da inviare nel messaggio.                30
297900070202L'ultimo carattere deve essere un (;)                                  31
298000100921é possibile solo se il VAB è fatto x la conferma  da CMR.              32
298100100921Nome CMR obbligatorio per le bolle x il raggruppamento x destinat.     33
298200160205Filiale errata inesistente                                             34
298300160209Filiale errata annullata o uguale alla LNP!                            35
