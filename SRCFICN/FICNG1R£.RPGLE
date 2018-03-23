000100120912     H DECEDIT('0,') DATEDIT(*DMY.)
000200120913     H dftactgrp(*no) actgrp(*caller)
000300120913     H BNDDIR('TIBS') EXTBININT(*YES) option(*nodebugio)
000400120912      *-----------------------------------------------
000500120907      /TITLE Gestione Firma x RF Valorizzazioni AUT/COOP/AFFDE
000600120907      *-----------------------------------------------
000700120910      *   il pgm richiamato da AZIONE ha nell'ultimo Byte della KPJBA il
000800120910      *   flag che identifica in quale modalità è stato chiamato.
000900120910      *   A = Città / C = Coop / D = Affluenze_Defluenze
001000120910      *-----------------------------------------------
001100120907      *  Nell'ottica di eliminare la stampa ed il consumo di carta,
001200120907      *  Si è gestita la Firma degli RF per le Valorizzazioni AUT/COP/AFFDEF
001300120907      *    Qui si lancia per tipo A=AUTCittà C=Coop D=AffDefl.
001400120907      *    l'elenco dell valorizzazioni/rettifiche da Firmare.
001500120907      *-----------------------------------------------
001600011221
001700130102     FTabel00F  IF   E           K DISK
001800121023     FTntbe01L  IF   E           K DISK
001900120912     FAZORG01L  IF   E           K DISK
002000120912     FANRCO98J  IF   E           K DISK
002100120907     Fficng1d   CF   E             WORKSTN
002200120907      *-----------------------------------------------
002300120912ab   ***************************************************************************
002400120912ab   ** Prototipi.
002500120912ab   ***************************************************************************
002600120912ab    **
002700120912ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
002800120912ab    /DEFINE DFTACTGRP_NO
002900120912ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
003000120912ab    /UNDEFINE DFTACTGRP_NO
003100120912ab   **
003200120912ab   ***************************************************************************
003300120912ab   **       Campi
003400120912ab   ***************************************************************************
003500120912ab   D TibsSof_esito...
003600120912ab   D                 S             10I 0 IMPORT
003700120912ab   D idSocieta...
003800120912EDPDCD                 S              3A
003900120912ab   **
004000130102     D ds3i          E DS
004100130102     D soctraini       S              3
004200120912EDPDCd Filiale         s              3S 0
004300121023     d DATA_VPO        s              8s 0
004400121023ab   **
004500120912EDPDCd dtVALIDITA      s               D
004600120912EDPDCd dtVALIDITAiniz  s               D
004700120912EDPDCd dataISO         s               D
004800120912EDPDCd VALdatFINE      s               D
004900120912EDPDCd VALdatINIZ      s               D
005000120912EDPDCd Data_FINE       s               D
005100120912EDPDCd Data_INIZIO     s               D
005200120912EDPDCd SOC_ERRATA      s              1
005300120912ab   ***************************************************************************
005400011221     D                SDS
005500011221     D PgmNam            *PROC
005600120911      **---   passaggio parametri
005700120907     D ficng1ds      E DS
005800120907      *
005900891030     D KPJBA         E DS
006000120912     D DKPJBU          s                   like(KPJBU)
006100120913     D savKpjbu        s                   like(KPJBU)
006200020318     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006300020318     D  LIN                    1     90  0
006400020318     D                                     DIM(30)                              FIL. COMODO
006500120907      *
006600020318     D ut§dse        E DS                  extname(ut§dse0f)
006700020318     D CNCR80        E DS
006800120907      *
006900131008     D tibs42ds      E DS
007000131008     D uteautds      E DS
007100121023     D dvpocont      E DS
007200120907      *
007300120912     D*------------------
007400120912     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
007500120912     D*------------------
007600120912     D PARAM           DS                  INZ
007700120912     D* TIPOLOGIA P_=autotrasport.
007800120912     D  PARTIP                 1      2
007900120912     D* RAGIONE SOCIALE aoutotras.
008000120912     D  PARRSC                 3     37
008100120912     D* S.I.
008200120912     D  PARCSF                38     40
008300120912     D* CODICE
008400120912     D  PARPDR                41     47  0
008500120912     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
008600120912     D*                  ALFABETICA RICHIESTA
008700120912     D  PARFLG                48     48
008800120912     D*------------------
008900020318     D WLBDAT          DS
009000020318     D  G02DAT                 1      8  0
009100020318     D  G02INV                 9     16  0
009200020318     D  G02ERR                17     17
009300020318     D  G02TGI                18     22  0
009400020308     D datiso          S               d
009500020308     D L1              S              3  0 DIM(30)
009600020308     D L6              S              3  0 DIM(30)
009700120913     D NO              C                   CONST('N')
009800120913     D SI              C                   CONST('S')
009900120910     D CITTA           C                   CONST('A')
010000120910     D COOP            C                   CONST('C')
010100120910     D AFFLUENZE       C                   CONST('D')
010200120912     D RESIDUALI       C                   CONST('P')
010300120913     D xAUTISTA        C                   CONST('AUTOTRASPORTATORI')
010400120914     D xCOOP           C                   CONST('   COOPERATIVE   ')
010500120914     D xAFFLDEFL       C                   CONST('AFFLUEN./DEFLUEN.')
010600120914     D xPRESTAZ        C                   CONST(' PREST.RESIDUALI ')
010700120907      *----------------------------------------------------------------
010800120910      *   Primo video - procedura di Firma Valorizzazioni/Rettifiche  *
010900120907      *----------------------------------------------------------------
011000120910     c*
011100120907     C                   EXSR      DEFCAM
011200120910     c*
011300120910     c*   cntrl se autorizzato
011400131014     C*                  EXSR      AUTORIZZATO
011500120907     c*
011600921222     C     *INKC         DOWEQ     '0'
011700020318     c* primo video
011800011221     C                   EVAL      *IN99 = *OFF
011900120911     c*
012000120907      *-
012100120907     C                   EXFMT     CNG1D1
012200020318     c* fine
012300120907     c   KC              leave
012400120913     c*
012500120913     c*   cntrl se autorizzato
012600120913     C                   EXSR      AUTORIZZATO
012700120907      *-
012800120907     c* ricerche sui campi
012900120927     C                   if        *inkd
013000120927     C                   do        *hival
013100120914     C                   EXSR      Ricerche
013200120927     c                   if        parflg = '3'
013300120914     c                   iter
013400120927     c                   else
013500120927     c                   leave
013600120927     c                   endif
013700120914     c                   endDO
013800120927     c                   endif
013900120907     c* controlli
014000120907     C                   EXSR      CTRD1
014100020318     c   99              iter
014200120907     c* conferma
014300120912     c                   clear                   ficng1ds
014400120912     c                   MOVEL     d1soc1        §G1SOC
014500120912     c                   MOVEL     d1fgs         §G1FGS
014600120912     c                   MOVEL     d1frn1        §G1PD1
014700120912     c                   MOVEL     d1frd1        §G1DP1
014800120912     c                   MOVEL     d1frn2        §G1PD2
014900120912     c                   MOVEL     d1frd2        §G1DP2
015000120912     c                   z-add     h1dadata      §G1DTI
015100120912     c                   z-add     h1adata       §G1DTF
015200120912     c                   MOVEL     d1tipo        §G1TIP
015300120912     c                   MOVEL     d1acco        §G1TSR
015400120912     c                   MOVEL     d1dafir       §G1FIR
015500120912     c                   MOVEL     *blank        §G1CMD
015600120907     c                   movel(p)  ficnG1ds      kpjbu
015700120914     c                   call      'FICNG2R'                            99
015800020319     c                   parm                    kpjba
015900120912     c                   movel     kpjbu         ficnG1ds
016000120912      * se deve andare a fine con F3
016100120912     c                   if        §g1CMD =  '03'
016200120912     c                   eval      *inKC = *ON
016300120912     c                   end
016400120907      **
016500020318     C                   ENDDO
016600120907      ****
016700921222     C                   SETON                                        LR
016800921222     C**************************************************************************
016900120907     C* Autorizzato a procedere  con il programma
017000020308     C**************************************************************************
017100120912     C     Autorizzato   BEGSR                                                  *
017200120907      **
017300120907     c* Controllo se abilitato alla stampa delle rettifiche
017400131008     c                   clear                   tibs42ds
017500131008     c                   move      d1fgs         i42pge
017700131008     c                   call      'TIBS42R'
017800131008     c                   parm                    tibs42ds
017900131008     c                   movel     o42uni        uteautds
018000120907      **
018100120907      **  Controlli di autorizzazione con i nuovi flags
018200120912     c                   if        §AUTRFCIT <> 'S' and d1tipo = CITTA
018300120910     c                                or
018400120912     c                             §AUTRFCOP <> 'S' and d1tipo = COOP
018500120910     c                                or
018600120912     c                             §AUTRFAFD <> 'S' and d1tipo = AFFLUENZE
018700120912     c                                or
018800120912     c                             §AUTRFPRE <> 'S' and d1tipo = RESIDUALI
018900120913     c                                or
019000120913     c                             §AUTRFCIT <> 'S' and §AUTRFCOP <> 'S' and
019100120913     c                             §AUTRFAFD <> 'S' and §AUTRFPRE <> 'S' and
019200120913     c                             tipresta = *blank
019300120907     c                   exfmt     MSGWIND
019400120907     c                   eval      *inKC = *ON
019401140108     c                   return
019500120907     c                   end
019600120913      **
019700120913      ** se uno è abilitato solo ad una funzione gliela preimposto
019800120913      **   città
019900120913     c                   if        §AUTRFCIT =  'S' and §AUTRFCOP <> 'S' and
020000120913     c                             §AUTRFAFD <> 'S' and §AUTRFPRE <> 'S' and
020100120913     c                             tipresta = *blank
020200120913     c                   eval      tipresta = 'A'
020300120913     c                   eval      d1tipo   = 'A'
020400120913     c                   end
020500120913      **   coop
020600120913     c                   if        §AUTRFCIT <> 'S' and §AUTRFCOP =  'S' and
020700120913     c                             §AUTRFAFD <> 'S' and §AUTRFPRE <> 'S' and
020800120913     c                             tipresta = *blank
020900120913     c                   eval      tipresta = 'C'
021000120913     c                   eval      d1tipo   = 'C'
021100120913     c                   end
021200120913      **   affl/defl
021300120913     c                   if        §AUTRFCIT <> 'S' and §AUTRFCOP <> 'S' and
021400120913     c                             §AUTRFAFD =  'S' and §AUTRFPRE <> 'S' and
021500120913     c                             tipresta = *blank
021600120913     c                   eval      tipresta = 'D'
021700120913     c                   eval      d1tipo   = 'D'
021800120913     c                   end
021900120913      **
022000120913      **   prestazioni residuali
022100120913     c                   if        §AUTRFCIT <> 'S' and §AUTRFCOP <> 'S' and
022200120913     c                             §AUTRFAFD <> 'S' and §AUTRFPRE =  'S' and
022300120913     c                             tipresta = *blank
022400120913     c                   eval      tipresta = 'P'
022500120913     c                   eval      d1tipo   = 'P'
022600120913     c                   end
022700120913      **
022800120907     C                   endSR                                                  *
022900120907     C**************************************************************************
023000120907     C* Ricerche sui campi a video
023100120907     C**************************************************************************
023200120907     C     Ricerche      BEGSR                                                  *
023300120907      **
023400120912     c                   IF        h1NMFL = 'D1FGS'
023500120912     c                   eval      d1fgs = FilProfUtente
023600120912      **
023700120912     c                   elseIF    h1NMFL = 'D1SOC1'
023800120912      **  reimposta  la società
023900120914     c                   if        h1adata >0
024000120912     C     *iso          move      h1adata       dtVALIDITA
024100120914     c                   else
024200120914     C     *iso          move      woggi         dtVALIDITA
024300120914     c                   end
024400120912     c                   move      D1FGS         FILIALE
024500120912     c                   exsr      chk_SOC_ope
024600120912     c                   eval      d1soc1= idsocieta
024700120912      *
024800120912     c                   elseIF    h1NMFL = 'D1FRN1'
024900120912     C*  deposita la KPJBU
025000120912     C                   MOVEL     KPJBU         DKPJBU          256
025100120912     C                   movel     d1soc1        parcsf
025200120914     c                   if        parcsf = *blank
025300120914     C                   movel     idsocieta     parcsf
025400120914     c                   end
025500120913     c                   clear                   nomFOR
025600120912     c                   exfmt     NOMEFRN
025700120912     C                   MOVEl     nomFOR        PARRSC
025800120912     C                   Z-ADD     *ZEROS        PARPDR
025900120912     C                   eval      partip = 'P '
026000120912     C                   MOVEL     *BLANKS       PARFLG
026100120912     c                   clear                   kpjbu
026200120912     C                   MOVEL(p)  PARAM         KPJBU
026300120912     C                   CALL      'TRUL16R'
026400120912     C                   PARM                    KPJBA
026500120912     C                   MOVEL     KPJBU         PARAM
026600120912IF  2C     PARFLG        IFEQ      '3'
026700120912     C                   MOVEL     *BLANKS       d1frn1
026800120912     C                   MOVEL     *all'?'       d1frd1
026900120912X   2C                   ELSE
027000120914     C                   movel     parcsf        d1soc1
027100120912     C                   MOVEl     *zeros        d1frn1
027200120912     C                   MOVE      PARPDR        d1frn1
027300120912     C                   MOVEL     PARRSC        d1frd1
027400120914     C                   write     CNG1D1
027500120912E   2C                   ENDIF
027600120912     C                   MOVEL     DKPJBU        KPJBU
027700120912      *
027800120912     c                   elseIF    h1NMFL = 'D1FRN2'
027900120912     C*  deposita la KPJBU
028000120912     C                   MOVEL     KPJBU         DKPJBU          256
028100120914     C                   movel     d1soc1        parcsf
028200120914     c                   if        parcsf = *blank
028300120914     C                   movel     idsocieta     parcsf
028400120914     c                   end
028500120913     c                   clear                   nomFOR
028600120912     c                   exfmt     NOMEFRN
028700120912     C                   MOVEl     nomFOR        PARRSC
028800120912     C                   Z-ADD     *ZEROS        PARPDR
028900120912     C                   eval      partip = 'P '
029000120912     C                   MOVEL     *BLANKS       PARFLG
029100120912     c                   clear                   kpjbu
029200120912     C                   MOVEL(p)  PARAM         KPJBU
029300120912     C                   CALL      'TRUL16R'
029400120912     C                   PARM                    KPJBA
029500120912     C                   MOVEL     KPJBU         PARAM
029600120912IF  2C     PARFLG        IFEQ      '3'
029700120912     C                   MOVEL     *BLANKS       d1frn2
029800120912     C                   MOVEL     *all'?'       d1frd2
029900120912X   2C                   ELSE
030000120914     C                   movel     parcsf        d1soc1
030100120912     C                   MOVEl     *zeros        d1frn2
030200120912     C                   MOVE      PARPDR        d1frn2
030300120912     C                   MOVEL     PARRSC        d1frd2
030400120914     C                   write     CNG1D1
030500120912E   2C                   ENDIF
030600120912     C                   MOVEL     DKPJBU        KPJBU
030700120912     C                   end                                                    *
030800120907      **
030900120907     C                   endSR                                                  *
031000120907     C**************************************************************************
031100120907     C* CONTROLLI VIDEO1
031200120907     C**************************************************************************
031300120912     C     CTRD1         BEGSR
031400120907      **
031500120914     c                   setoff                                       41
031600120913      *        decodifica
031700120913     c                   if        d1tipo = CITTA
031800120913     c                   eval      tipologia =  xAUTISTA
031900120913     c                   elseif    d1tipo = COOP
032000120913     c                   eval      tipologia =  xCOOP
032100120913     c                   elseif    d1tipo = AFFLUENZE
032200120913     c                   eval      tipologia =  xAFFLDEFL
032300120913     c                   elseif    d1tipo = RESIDUALI
032400120913     c                   eval      tipologia =  xPRESTAZ
032500120913     c                   elseif    d1tipo = *blank
032600120914     C                   SETON                                        119941
032700120913     c   99              leavesr
032800120913     c                   end
032900921224     C                   SETOFF                                       99
033000120910     C* Filiale Autista
033100120912     C     d1fgs         CHAIN     AZORG01L
033200120912     c                   if        %Found(azorg01l)
033300120912     c                   eval      D1FGSD = orgDES
033400120912     c                   else
033500120912     C                   SETON                                        0199
033600120912     c                   eval      D1FGSD = *all'?'
033700120912     c   99              leavesr
033800120912     c                   end
033900120907      **
034000120914     C* Verifico esistenza filiale autista: se REM su £6, se SIMFEL su £1
034100120914     c                   if        utente_EDP <> 'S'
034200120914      **
034300120914      ** controllo sulla £6 sia x primi che secondi Livelli
034400120914     C     d1fgs         LOOKUP    L6                                     20
034500120914     C  n20              SETON                                        0299
034600120914     c   99              leavesr
034700120914      **
034800120914     c                   else
034900120914      **
035000120914      ** utente EDP SEDE non controlla nulla
035100121106     c*********          if        simpou <> 046
035200121106     C**** d1fgs         LOOKUP    L1                                     20
035300121106     C**N20              SETON                                        0299
035400121106     c***99              leavesr
035500121106     c*********          end
035600121106      **
035700120914     c                   end
035800120912     C* DADATA
035900120912    1C                   IF          d1Dadata >0 AND d1Dadata < 99999999
036000120912     C                   Z-ADD     d1Dadata      G02DAT
036100120912     C                   MOVEL     *BLANK        G02ERR
036200120912     C                   CALL      'XSRDA8'
036300120912     C                   PARM                    WLBDAT
036400120912    2C     G02ERR        IFEQ      '1'
036500120912     C                   SETON                                        0399
036600120912     c   99              leavesr
036700120912    2C                   Else
036800120912     C                   move      G02dat        d1Dadata
036900120912     C                   move      G02INV        datiso
037000120912     c                   move      datiso        h1Dadata
037100120912    1C                   END
037200120912    1C                   ENDIF
037300121024      **
037400121024      **  La DATA di inizio Periodo NON è a video ed è sempre impostata a (0).
037500121024      **    A questo punto impostando la data presente sulla VPO determino il
037600121024      **      Limite dal quale iniziare a controllare i dati x la Firma.
037700121024      **
037800121024      **    In ogni caso la data della VPO è controllata e confrontata con
037900121024      **     La DATA di FINE Periodo.
038000121024      **    Che deve essere successiva alla data di Partenza della VPO altrimenti errore.
038100121024     c                   if        h1dadata = 0
038200121024     c                   eval         h1dadata = data_VPO
038300121024     c                   end
038400120912      **
038500120912     C* ADATA
038600120914    1C                   IF          d1adata =0
038700120914     C                   SETON                                        0499
038800120914     c   99              leavesr
038900120920     c                   else
039000120912     C                   Z-ADD     d1adata       G02DAT
039100120912     C                   MOVEL     *BLANK        G02ERR
039200120912     C                   CALL      'XSRDA8'
039300120912     C                   PARM                    WLBDAT
039400120912    2C     G02ERR        IFEQ      '1'
039500120912     C                   SETON                                        0499
039600120912     c   99              leavesr
039700120912    2C                   Else
039800120912     C                   move      G02dat        d1adata
039900120912     C                   move      G02INV        datiso
040000120912     c                   move      datiso        h1adata
040100120920     c                   if        h1adata > woggi
040200120920     C                   SETON                                        1499
040300120920     c   99              leavesr
040400120920     c                   end
040500120912    1C                   ENDIF
040600120914     c                   end
040700121024      **
040800120912      **
040900120912     C* DATE NON ORDINATE CORRETTAMENTE
041000121024     C                   IF        h1ADATA < h1DADATA
041100120912     C                   SETON                                        0599
041200120912     c   99              leavesr
041300120912    1C                   ENDIF
041400120912      **
041500120914     C*  NON SI DEVE CONTROLLARE LA SOCIETA' se non IMMESSA
041600120912     C*  Codice Società esistente
041700120914     c                   if        d1soc1 <> *blank and h1adata > 0
041800120912     C     *iso          move      h1adata       dtVALIDITA
041900120912     c                   move      d1fgs         FILIALE
042000120912     c                   exsr      chk_SOC_ope
042100120912     c                   if        d1soc1 <> idsocieta
042200130102     c                             and (d1soc1 <> soctraini and D1TIPO ='D'
042300130102     c                                  or D1TIPO <>'D')
042400120912     C                   SETON                                        0699
042500120912     c   99              leavesr
042600120912     c                   end
042700120914     c                   end
042800120910     C*
042900120910     C*  Codice Fornitore esistente
043000120914     C*    Fornitore deve corrispondere all'unità (filiale) selezionata
043100120914     c                   clear                   d1frd1
043200120914     c                   clear                   d1fil1            3
043300120913     c                   if        d1frn1 <> *blank and d1frn1 <> *zeros
043400120914      ****
043500120914     c                   eval      d1fil1 = %subst(d1frn1:2:3)
043600120914     c                   move      d1fgs         campo3            3
043700120914     c                   if        d1fil1 <> campo3
043800120914     C                   SETON                                        1299
043900120914     c                   end
044000120914      ****
044100120912     c                   movel     d1soc1        RCOSOCIETA
044200120914     c                   if          d1soc1 = *blank
044300120914     c                   movel     idsocieta     RCOSOCIETA
044400120914     c                   end
044500120912     c                   move      *all'0'       RCOKSC
044600120912     c                   move      d1frn1        RCOKSC
044700120912     C                   move      'F'           subnatura
044800120912     C     Krco98j       chain     anRCO98j
044900120912     c                   if        not %Found(anRCO98j)
045000120914     c                   eval      d1frd1 = *ALL'?'
045100120912     c                   else
045200120912     c                   eval      d1frd1 = SOGDES
045300120912     c                   endif
045400120910     c                   end
045500120912     C*
045600120912     C*  Codice Società esistente
045700120914     c                   if        d1soc1 <> *blank and h1adata > 0
045800120912     C     *iso          move      h1adata       dtVALIDITA
045900120912     c                   move      d1fgs         FILIALE
046000120912     c                   exsr      chk_SOC_ope
046100120914     c                   if        d1soc1 <> idsocieta
046200130102     c                             and (d1soc1 <> soctraini and D1TIPO ='D'
046300130102     c                                  or D1TIPO <>'D')
046400120912     C                   SETON                                        0899
046500120912     c   99              leavesr
046600120912     c                   end
046700120914     c                   end
046800120910     C*
046900120910     C*  Codice Fornitore esistente
047000120914     C*    Fornitore deve corrispondere all'unità (filiale) selezionata
047100120914     c                   clear                   d1frd2
047200120914     c                   clear                   d1fil2            3
047300120914      *
047400120914     c                   if        d1frn2 <> *all'9' AND D1FRN2 <> *ALL'0'
047500120914      ****
047600120914     c                   eval      d1fil2 = %subst(d1frn2:2:3)
047700120914     c                   move      d1fgs         campo3            3
047800120914     c                   if        d1fil2 <> campo3
047900120914     C                   SETON                                        1399
048000120914     c                   end
048100120914      ****
048200120914     c                   movel     d1soc1        RCOSOCIETA
048300120914     c                   if          d1soc1 = *blank
048400120914     c                   movel     idsocieta     RCOSOCIETA
048500120914     c                   end
048600120912     c                   move      *all'0'       RCOKSC
048700120912     c                   move      d1frn2        RCOKSC
048800120912     C                   move      'F'           subnatura
048900120912     C     Krco98j       chain     anRCO98j
049000120912     c                   if        not %Found(anRCO98j)
049100120914     c                   eval      d1frd1 = *ALL'?'
049200120912     c                   else
049300120912     c                   eval      d1frd2 = SOGDES
049400120912     c                   endif
049500120912     c                   end
049600120914      **
049700120914     c                   if        d1frn1 = *all'0' AND D1FRN2 = *ALL'0'
049800120914     c                                or
049900120914     c                             d1frn1 = *all'0' AND D1FRN2 = *ALL'9'
050000120914     c                   eval        D1FRN2 = *ALL'9'
050100120914     c                   eval        D1FRd1 = 'Tutti'
050200120914     c                   eval        D1FRd2 = 'Tutti'
050300120914     c                   end
050400120910      **
050500120910     C* Codici Fornitori NON nella sequenza giusta
050600120912     C                   IF        d1frn2 < D1Frn1
050700120912     C                   SETON                                        1099
050800120912     c   99              leavesr
050900120910    1C                   ENDIF
051000120914     C*
051100120914     c* TIPO PRESTAZIONE:
051200120914      *- se già ha ricevuto l'informazione da menù x sapere la modalità
051300120914     c*   con cui è stato chiamato, protegge il campo di scelta.
051400120914     c                   setoff                                       4445
051500120914     c                   if          tipresta <> *blank and
051600120914     c                             d1tipo <> *blank
051700120914     c                   seton                                        44
051800120914     c                   endif
051900120914     c*  solo x coop altrimenti protegge l'accorpamento
052000120914     c                   if        d1tipo <> 'C'
052100120914     c                   seton                                        45
052200120914     c                   end
052300120912     C*
052400120912     C     enderror      ENDSR
052500020308     C**************************************************************************
052600921222     C* INIZIALIZZAZIONE
052700020308     C**************************************************************************
052800921223     C     DEFCAM        BEGSR                                                  *
052900121023      *
053000121023     C     Ktbe          KLIST
053100121023     C                   KFLD                    tbecod
053200121023     C                   KFLD                    tbeke1
053300121023     C                   KFLD                    tbeke2
053400120912     C*
053500120912     C     Krco98j       KLIST
053600120912     C                   KFLD                    RCOSOCIETA
053700120912     C                   KFLD                    subnatura         1
053800120912     C                   KFLD                    RCOKSC
053900130102      *
054000130102     C     Ktab          KLIST
054100130102     C                   KFLD                    tblkut
054200130102     C                   KFLD                    tblcod
054300130102     C                   KFLD                    tblkey
054400120912      *
054500921223     C     *ENTRY        PLIST                                                  *
054600921223     C                   PARM                    KPJBA                          *
054700120913     C                   movel     kpjbu         savkpjbu                       *
054800130102      *
054900130102     C                   z-add     1             tblkut
055000130102     c                   move      '3I'          tblcod
055100130102     c                   movel(p)  '1'           tblkey
055200130102     c     ktab          chain     tabel00f
055300130102     c                   if        %found(tabel00f)
055400130102     c                   movel     tbluni        ds3i
055500130102     c                   eval      soctraini = %subst(§3IBST: 7: 3)
055600130102     c                   endif
055700130102      *
055800120913     C*     Inizializza
055900120913      /FREE
056000120913       TibsSof_Init();
056100120913      /END-FREE
056200120913      *
056300120910     C***   flag di ingresso x identificare come è stato chiamato
056400120910      *       come CITTA o COOP o AFFLUENZE DEFLUENZE
056500120911     C                   move      KPJBA         tipresta          1
056600120910      *
056700120910      * Recupero data e ora x messaggio
056800120910     C                   TIME                    WHHDAT           14 0
056900120910     C                   MOVE      WHHDAT        G02DAT
057000120910     C                   MOVE      WHHDAT        DATA8             8 0
057100120910     C                   MOVEL     *BLANK        G02ERR
057200120910     C                   CALL      'XSRDA8'
057300120912     C                   PARM                    WLBDAT
057400120910     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
057500120910     C                   MOVEL     WHHDAT        WHHMM             4 0
057600120910     C                   MOVE      WHHDAT        WANNO             2 0
057700120910     C                   Z-ADD     woggi         WOGGIymd          6 0           UDATE
057800120910     C***
057900020308     C                   Z-ADD     1             CODUT
058000020308     C                   CALL      'X§PARUT'
058100020308     C                   PARM                    UT§DSE
058200020322     C                   MOVEL     RAGUT         RSUT
058300020308     C                   MOVEL     REC80         CNCR80
058400120910     C                   Z-ADD     SimPOU        FilProfUtente     3 0
058500120912     C                   Z-ADD     SimFEL        FilElaboratore    3 0
058600120907     C***
058700120907     c                   clear                   ficng1ds
058800020308     C***
058900121023     c* tabbella VPO record CONT
059000121023     C                   movel     'VPO'         tbecod
059100121023     C                   MOVEL(P)  'CONT'        tbeke1
059200121023     C                   clear                   tbeke2
059300121023     C                   clear                   dvpocont
059400121023     C     KTbe          CHAIN     Tntbe01l
059500121023     c                   if        %found(tntbe01l)
059600121023     C                   movel     tbeuni        dvpocont
059700121023     C                   movel     §VPODFIR      data_VPO
059800121023     c                   else
059900121023     c                   clear                   dvpocont
060000121023     c                   end
060100121023     C*
060200020308     C* CARICO TABELLA FILIALI GESTITE £1
060300020308     C                   CLEAR                   DSUL06
060400020308     C                   MOVE      '£1'          D06COD
060500120913     C                   MOVEL     FilProfUtente D06KEY
060600020308     C                   MOVEL     ' '           D06TLA
060700120913     C                   MOVEL     KPJBU         DKPJBU
060800120913     c                   clear                   kpjbu
060900020308     C                   MOVEL     DSUL06        KPJBU
061000020308     C                   CALL      'TRUL06R'
061100020308     C                   PARM                    KPJBA
061200020308     C                   MOVEL     KPJBU         DSUL06
061300120913     C                   MOVEL     dKPJBU        KPJBU
061400020308     C                   MOVEA     LIN           L1
061500020321     C***
061600020321     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
061700020321     C                   CLEAR                   DSUL06
061800020321     C                   MOVE      '£6'          D06COD
061900120912     C                   MOVEL     FilProfUtente D06KEY
062000020321     C                   MOVEL     'L'           D06TLA
062100120913     C                   MOVEL     KPJBU         DKPJBU
062200120913     c                   clear                   kpjbu
062300020321     C                   MOVEL     DSUL06        KPJBU
062400020321     C                   CALL      'TRUL06R'
062500020321     C                   PARM                    KPJBA
062600020321     C                   MOVEL     KPJBU         DSUL06
062700120913     C                   MOVEL     dKPJBU        KPJBU
062800020321     C                   MOVEA     LIN           L6
062900120913     C***
063000120913     C                   EXSR      INZD01
063100021111     C*                                                    *
063200921223     C                   ENDSR
063300020308     C*---------------------------------------------------------------*
063400020308     C* PULISCO 1 FORMATO --------------------------------------------*
063500020308     C*---------------------------------------------------------------*
063600120910     C     INZD01        BEGSR
063700120912      *
063800120912     C     *iso          move      woggi         dtVALIDITA
063900120912     c                   move      FilProfUtente FILIALE
064000120912     c                   exsr      chk_SOC_ope
064100120910      *
064200120914     c                   setoff                                       41
064300120912     c                   eval      d1tipo = tipresta
064400120913     c                   if        d1tipo = CITTA
064500120913     c                   eval      tipologia =  xAUTISTA
064600120913     c                   elseif    d1tipo = COOP
064700120913     c                   eval      tipologia =  xCOOP
064800120913     c                   elseif    d1tipo = AFFLUENZE
064900120913     c                   eval      tipologia =  xAFFLDEFL
065000120913     c                   elseif    d1tipo = RESIDUALI
065100120913     c                   eval      tipologia =  xPRESTAZ
065200120914     c                   elseif    d1tipo = *blank
065300120914     c                   seton                                        41
065400120913     c                   end
065500120912     C                   move      FilProfUtente D1FGS
065600120914     c*****              move      idSocieta     D1SOC1
065700120914     c                   move      *blank        D1SOC1
065800120912     c                   move      *all'0'       D1FRN1
065900120914     c                   movel(p)  'Tutti'       D1FRD1
066000120912     c                   move      *all'9'       D1FRN2
066100120914     c                   movel(p)  'Tutti'       D1FRD2
066200120914      *
066300120912     C                   move      *all'0'       d1dadata
066400120912     C                   move      *all'0'       h1dadata
066500120914      *
066600120914     c*******            z-add     data8         D1ADATA
066700120914     c*******            z-add     woggi         H1ADATA
066800120914     c                   z-add     0             D1ADATA
066900120914     c                   z-add     0             H1ADATA
067000120914      *
067100120912     c                   move      'S'           D1dafir
067200120912     c                   move      'N'           D1ACCO
067300120910      *
067400120914     c                   clear                   utente_EDP        1
067500120914      * Protegge la filiale di gestione  solo x i 2° livello
067600120912     c                   setoff                                       43
067700120914     c                   if        %subst(Knmus:1:3) = 'EDP' or
067800120914     C                             SimTpP = '1'
067900120914     c                   seton                                        43
068000120914     c                   if        %subst(Knmus:1:3) = 'EDP'
068100120914     c                   move      'S'           utente_EDP
068200120912     c                   end
068300120914     c                   end
068400120912      *
068500120914     c                   setoff                                       4445
068600120914      *
068700020308     C                   ENDSR
068800120912ab   C**************************************************************************
068900120912ab    * Controlla la Società operativa della Filiale
069000120912ab   C**************************************************************************
069100120912ab   C     CHK_SOC_OPE   begsr
069200120912ab   **
069300120912     c                   eval      soc_errata = NO
069400120912ab    /FREE
069500120912EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
069600120912EDPDC                                             : dtVALIDITA
069700120912EDPDC                                             : 'O'
069800120912EDPDC                                             : valDatIniz
069900120912EDPDC                                             : valDatFine
070000120912ab                                                );
070100120912ab
070200120912EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
070300120912EDPDC    // Gestire l'errore.
070400120912         eval  soc_errata = SI;
070500120912ab     ENDIF;
070600120912ab
070700120912ab    /END-FREE
070800120912ab   C                   ENDSR
070900120912     C**************************************************************************
