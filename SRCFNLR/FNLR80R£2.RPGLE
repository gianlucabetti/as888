000100120712     /*PRM dbgview(*source)
000200120712     /*CMD ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000300120712     /*CMD        ovrscope(*calllvl)
000400120712     /*END dltovr file(FIAR531C) lvl(*)
000500120712     /*END
000600040308     H DECEDIT('0,') DATEDIT(*YMD.)
000700120710     h option(*nodebugio)
000800120710     h dftactgrp(*no)
000900120710     h bnddir('UBRTVNETA')
001000120710      *---------*
001100120710      * FNLR80R *----------------------------------------------------*
001200120710      *---------*                                                    *
001300120710      *         - ELENCO BOLLE CON OBBLIGO RESTITUZIONE -            *
001400120710      *            - BAM FIRMATA/LETTERA DI VETTURA -                *
001500120710      *--------------------------------------------------------------*
001600120920     FFNARBX4L  UF   E           K DISK    USROPN
001700960117     F*---------
001800000221     FFNARBX3L  UF   E           K DISK    USROPN
001900000221     F                                     RENAME(FNARBX00:FNARBX3)
002000000221     F*---------
002100960117     FFNARB01L  IF   E           K DISK
002200960117     F*---------
002300040316     FFnLBL01L  IF   E           K DISK
002400040316     F*---------
002500040316     FFnart01L  IF   E           K DISK
002600040303     F*---------
002700050309     Ftigcp21L  IF   E           K DISK
002800040303     F*---------
002900040303     FFiar501L  IF   E           K DISK
003000120712     f*---------
003100120712     fFIAR531C  IF   E           K DISK    usropn
003200120712     f                                     extfile(wLibFIAR5)
003300120712     f                                     rename(FIAR5000:FIAR500S)
003400120712     f                                     rename(FIAR5P00:FIAR5P0S)
003500040303     F*---------
003600960117     FTABEL00F  IF   E           K DISK
003700120710     f*---------
003800120710     fTNTBE01L  if   e           k disk    usropn
003900120710     f                                     extfile(wLibTNTBE)
004000960117     F*---------
004100960117     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
004200960117     F*---------
004300991130     FFNLR80P   O    E             PRINTER OFLIND(*IN04)
004400120625      *--------------------------------------------------------------*
004500120625      *   c o s t a n t i                                            *
004600120625      *--------------------------------------------------------------*
004700120625     D CPERDP          C                   CONST('   DA INVIARE   IN -
004800120625     D                                        SEDE')
004900120625     D CNOBOL          C                   CONST('NESSUNA BOLLA')
005000120710
005100120710       //--------------------------------------------------------------
005200120710       //?Definizione aree dati.                                       ?
005300120710       //--------------------------------------------------------------
005400120710
005500120710       // -?Dati utente?
005600120710     d §AzUte        e ds                  extname(AZUTE00F)
005700120710     d                                     dtaara
005800120710     d §DatiUte      e ds                  extname(dDatiUte)
005900120710     d                                     dtaara
006000960117      *--------------------------------------------------------------*
006100960117      *   s c h i e r e                                              *
006200960117      *--------------------------------------------------------------*
006300960117     D C7R             S              2    DIM(100)                             SKI CHIAVI 7R
006400960117     D T7R             S              1    DIM(100)                             STP.BAM/L.VETT
006500040319     D C7Q             S              2    DIM(100)                             SKI CHIAVI 7R
006600040319     D D7Q             S             89    DIM(100)                             STP.BAM/L.VETT
006700040319     D D7W             S             89    DIM(100)                             STP.BAM/L.VETT
006800120625     d $3A             S                   dim(100) like(ARBcbo)
006900120625      *--------------------------------------------------------------*
007000120625      *   s t r u t t u r e   d a t i                                *
007100120625      *--------------------------------------------------------------*
007200120706     D FNLR80DS      E DS
007300120709     D DAR5DOC       E DS                  inz   qualified
007400040303     D DAR5GEN       E DS
007500120625     d ds3A          e ds                  inz
007600040319     D Ds7w          E DS
007700040319     D Ds7q          E DS
007800120625     D DS7R          E DS
007900120920      * -?Campo FNARBX0F.ARBFLR?
008000120920     d dARBflrX      e ds                  inz  qualified
008100120625      * -?Tab. VPO = Variabili Programmi Operativi?
008200120625     d dVPO          e ds                  inz
008300120625      * -?Tab. JDC = Clienti x ritorno immagini DOC?
008400120625     d dJDC          e ds                  inz
008500120625      *
008600960117     D WLBDA8          DS
008700960117     D  G02DAT                 1      8  0
008800960117     D  G02INV                 9     16  0
008900960117     D  G02ERR                17     17
009000960117     D  G02TGI                18     22  0
009100040304      *
009200040304      * Stampa dettaglio colli non sequenziali
009300040304     D  dsNCL          DS
009400040304     d stcnc0                  1      7  0
009500040304     d stcnc1                  8     14  0
009600040304     d stcnc2                 15     21  0
009700040304     d stcnc3                 22     28  0
009800040304     d stcnc4                 29     35  0
009900040304     d stcnc5                 36     42  0
010000040304     d stcnc6                 43     49  0
010100040304     d stcnc7                 50     56  0
010200040304     d stcnc8                 57     63  0
010300040304     d stcnc9                 64     70  0
010400040304     d stcncA                 71     77  0
010500040304     d stcncB                 78     84  0
010600040304     d stcncC                 85     91  0
010700040304     d stcncD                 92     98  0
010800040304     d stcncE                 99    105  0
010900040304     D    NCL                  1    105  0 DIM(15)
011000040304      *
011100040304      * Data attivazione scan POD - LDV EUROEXPRESS
011200040316     D OG146         E DS                  prefix(§146_)
011300040316     D OG143         E DS                  prefix(§143_)
011400040304      *
011500960117     D KPJBA         E DS
011600120625      *
011700960117     D                SDS
011800960117     D  NOMPGM                 1     10
011900120625      *--------------------------------------------------------------*
012000120625      *   v a r i a b i l i                                          *
012100120625      *--------------------------------------------------------------*
012200120712     d $CallTNSB25     s               n   inz
012300120911     d*// $in07parz       s               n   inz
012400120625     d wCount          s              5  0 inz
012500120710     d Udate8          s              8  0
012600120920     d WsvLNPx         s                   like(dARBflrX.ARBlnpX)  inz
012700120710
012800120710       // -?Nome esteso Libreria/File del TNTBE01L di Sede?
012900120710     d wLibTNTBE       s             21a   inz
013000120712       // -?Nome esteso Libreria/File del FIAR531C di Sede?
013100120712     d wLibFIAR5       s             21a   inz
013200120625
013300120625       //--------------------------------------------------------------
013400120625       //?Definizione prototipi procedure.                             ?
013500120625       //--------------------------------------------------------------
013600120710
013700120710       // -?Nome del sistema?
013800120710     d currSysNeta     s              8a   inz
013900120710       // -?Reperimento NETA sistema AS/400 corrente?
014000120710      /copy gaitrasrc/srcProtoPR,UBRTVNETA
014100120625
014200120710       // -?Reperimento dati utente?
014300120710     d TIBS34ds      e ds
014400120710      /copy gaitrasrc/srcProtoPR,TIBS34R
014500120710
014600120625       // -?Ricerca/controllo tabelle?
014700120625     d TIBS02ds      e ds                  inz
014800120625      /copy gaitrasrc/srcProtoPR,TIBS02R
014900120625
015000120625       // -?Stampa copia LdV x Scannerizzazione cocumenti clienti?
015100120625     d TNSB25ds      e ds                  inz
015200120625     d   D25ges      e                     inz('T')
015300120625      /copy gaitrasrc/srcProtoPR,TNSB25R
015400120710
015500120710       //--------------------------------------------------------------
015600120710       //?Definizione key-list.                                        ?
015700120710       //--------------------------------------------------------------
015800120710
015900120710       // -?File TNTBE01L?
016000120710     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
016100120710     d                                     prefix(k_)   inz
016200120625
016300960117      *--------------------------------------------------------------*
016400960117      *   D s                                                        *
016500960117      *--------------------------------------------------------------*
016600960117     IAZORG01L  AA
016700960117     I                             P    4    5 0ORGFIL
016800960117     I                                 14   33  ORGDES
016900040305     I                               3776 3800  ORGDE3
017000040305     I                               3851 3875  ORGDE6
017100960117      *--------------------------------------------------------------*
017200960117      *   C I C L O    P R I N C I P A L E                           *
017300960117      *--------------------------------------------------------------*
017400120710     C                   SETON                                        LR
017500960117      *  Leggo record con data di stampa bolla uguale a quella rich.
017600000221      *  ... se DPD ordinate x numero distinta
017700000221     C     D80DPD        IFEQ      'S'
017800120709     C                   exsr      sr_ElabDPD
017900000221      *  ... se non DPD ordinate x numero spedizione
018000000221     C                   ELSE
018100120709     C                   exsr      sr_ElabBRT
018200000221     C                   END
018300960117      *
018400960117      *  Se sono in stampa richiamo pgm. pulizia
018500960117     C     D80RIS        IFNE      'S'
018600960117     C                   CALL      'FNLR80R1'
018700960117     C                   PARM                    KPJBA
018800960117     C                   END
018900960117      *
019000960117     C     FINE          TAG
019100120710     C                   return
019200120709      *--------------------------------------------------------------*
019300120709      *  sr_ElabDPD: Elaborazione x DPD                              *
019400120709      *--------------------------------------------------------------*
019500120709     C     sr_ElabDPD    BEGSR
019600120709      *
019700120709      *  Leggo record con data di stampa bolla uguale a quella rich.
019800120709      *  ... se DPD ordinate x numero distinta
019900120709     C                   Z-ADD     D80FGS        KFGS
020000120709     C                   Z-ADD     D80DST        KDST
020100120709     C     KARBX         CHAIN     FNARBX3L                           30
020200120709     C     *IN30         DOWEQ     '0'
020300120709      * testa tipologie valide
020400120709     c                   if        arbtpl = ' ' or arbtpl = 'C'
020500120709      *
020600120709      *  Attenzione: per le filiali che hanno iniziato la scannerizzazione
020700120709      *   dei POD non si deve stampare nulla e cancellare i records dal file
020800120709      *    spia ARBX....         Data inizio scannerizzazione
020900120709     c                   move      'S'           stampaE           1
021000120709     c                   move      *zeros        data_attiv        8 0
021100120709     c                   move      *zeros        lblLPO
021200120709      *
021300120709     C                   IF        §146_§ogDSD <> *blank or
021400120709     C                             §146_§ogDSE <> *blank
021500120709      *
021600120709     C     ArbLNP        CHAIN     AZORG01L                           40
021700120709      *
021800120709     c     *in40         ifeq      *off
021900120709     C                   MOVEL     ORGDE3        OG143
022000120709      *
022100120709      * Attenzione: se bolla non estero e dirottata occorre controllare la
022200120709      *  bolla originale
022300120709      *   x vedere se non deve essere stampata la figlia....
022400120709     c                   if        §143_§OGNTW <> 'EEX' and
022500120709     c                             §143_§OGNTW <> 'DPD'
022600120709      * file legami con bolla originale
022700120709     C     KLBL          chain     fnLBL000
022800120709     c                   if        %Found(fnLBL01L) and LBLLPO <> arblnp
022900120709      * con la bolla originale si controlla se linea estera
023000120709     C     LBLlpo        CHAIN     AZORG01L
023100120709     c                   if        %Found(azorg01l)
023200120709     C                   MOVEL     ORGDE3        OG143
023300120709     c                   end
023400120709     c                   end
023500120709     c                   end
023600120709      *
023700120709      * Euroexpress
023800120709     c     §143_§ogNTW   ifeq      'EEX'
023900120709     c     §146_§ogDSE   andne     *blank
024000120709     c     §146_§ogDSE   andne     *all'0'
024100120709     c                   move      §146_§ogDSE   data_attiv        8 0
024200120709     c                   end
024300120709      *
024400120709      * Chiodo per Timberland le cui spedizioni hanno LNP = 89 ma fa parte
024500120709      * del circuito Euroexpress
024600120709     c     §146_§ogDSE   ifne      *blank
024700120709     c     §146_§ogDSE   andne     *all'0'
024800120709     c     arbLNP        ifeq      089
024900120709     c     lblLPO        oreq      089
025000120709     c                   move      §146_§ogDSE   data_attiv        8 0
025100120709     c                   end
025200120709     c                   end
025300120709      * Dpd
025400120709     c     §143_§ogNTW   ifeq      'DPD'
025500120709     c     §146_§ogDSD   andne     *blank
025600120709     c     §146_§ogDSD   andne     *all'0'
025700120709     c                   move      §146_§ogDSD   data_attiv
025800120709     c                   end
025900120709      *
026000120709      * Se la data di attivazione è attiva non deve stampare
026100120709     c     data_Attiv    ifle      udate8
026200120709     c     data_Attiv    andne     *zeros
026300120709     c                   move      'N'           stampaE           1
026400120709     c                   end
026500120709      *
026600120709     c                   end
026700120709     c                   endIF
026800120709      *
026900120709      * Solo se non è stato disabilitato
027000120709     c                   if        StampaE ='S'
027100120709      *  Stampo dati bolla
027200120709     C                   EXSR      STPBOL
027300120709     c                   endIF
027400120709      *endif tipologie
027500120709     c                   endif
027600120709      *  Lettura successiva
027700120709     C     KARBX         READE     FNARBX3L                               30
027800120709     C                   END
027900120709     C* STAMPA TOTALI
028000120709     C     STTOTB        IFEQ      0
028100120709     C                   WRITE     TES01
028200120709     c                   write     Tes02
028300120709     c                   write     Tes04
028400120709     C                   MOVEL     CNOBOL        STMSG1
028500120709     C                   ENDIF
028600120709     C                   WRITE     TOT01
028700120709      *
028800120709     C                   ENDSR
028900120709      *--------------------------------------------------------------*
029000120709      *  sr_ElabDPD: Elaborazione x BRT                              *
029100120709      *--------------------------------------------------------------*
029200120709     C     sr_ElabBRT    BEGSR
029300120709      *
029400120709      *  Leggo record con data di stampa bolla uguale a quella rich.
029500120709      *  ... se non DPD ordinate x numero spedizione
029600120709     C                   Z-ADD     D80FGS        KFGS
029700120709     C                   Z-ADD     D80DST        KDST
029800120920     C     KARBX         CHAIN     FNARBX4L                           30
029900120709     C     *IN30         DOWEQ     '0'
030000120709      * testa tipologie valide
030100120709     c                   if        arbtpl = ' ' or arbtpl = 'C'
030200120709      *
030300120709      *  Stampo dati bolla
030400120709      *      SE MERCE DA DISTRUGGERE
030500120709     C     D80DPD        IFEQ      'X'
030600120709     C     ARBTPL        ANDEQ     'C'
030700120709     C                   EXSR      STPBOL
030800120709     C                   ENDIF
030900120709      *
031000120709      *      SE BOLLE FIRMATE
031100120709     C     D80DPD        IFNE      'X'
031200120709     C     ARBTPL        ANDEQ     ' '
031300120709     C                   EXSR      STPBOL
031400120709     C                   ENDIF
031500120709      * endif tipologie
031600120709     c                   endif
031700120709      *  Lettura successiva
031800120920     C     KARBX         READE     FNARBX4L                               30
031900120709     C                   END
032000120709      *
032100120709     C                   ENDSR
032200960117      *--------------------------------------------------------------*
032300120709      *  STPBOL: Stampa bolla (filtro)                               *
032400960117      *--------------------------------------------------------------*
032500960117     C     STPBOL        BEGSR
032600120920      *
032700120920     c                   eval      dARBflrX = ARBflr
032800120920      *?TEMP x TEST (finché questo campo non sarà SEMPRE valorizzato)?
032900120920     c                   if        dARBflrX.ARBlnpX <= *zero
033000120920     c                   eval      dARBflrX.ARBlnpX = %editc( ARBlnp : 'X' )
033100120920     c                   endif
033200960117      *
033300040305      *  Esegue rotture di livello solo se non si tratta di merce da
033400040305      *    distruggere.
033500040305     c                   if        arbtpl <> 'C'
033600991222      *  Eseguo posizionamento solo se bolla  è cambiata
033700120920     C*/N05ARBLNP        COMP      WSVLNP                             0505
033800120920     c                   if        Not *in05
033900120920     c                   eval      *in05 = (dARBflrX.ARBlnpX <> WsvLNPx)
034000120920     c                   endif
034100000225     C  N06ARBDDC        COMP      WSVDDC                             0606
034200120706     c                   endif
034300040305      *
034400000225     C                   Z-ADD     ARBDDC        WSVDDC            8 0
034500000225      *  Giro la data distinta di consengna
034600000225     C                   Z-ADD     0             STCDDC            8 0
034700000225     C     ARBDDC        IFNE      0
034800000225     C                   Z-ADD     ARBDDC        G02INV
034900000225     C                   MOVEL     '3'           G02ERR
035000000225     C                   CALL      'XSRDA8'
035100000225     C                   PARM                    WLBDA8
035200000225     C                   Z-ADD     G02DAT        STCDDC            8 0
035300000225     C                   Z-ADD     ARBNDC        STCNDC
035400120706     C                   ENDIF
035500991222      *  ABBLENCO FLAG DI STAMPA
035600991222     C                   CLEAR                   WSTAMP
035700991222      *
035800120920     c                   eval      WsvLNPx = dARBflrX.ARBlnpX
035900960117     C                   Z-ADD     ARBAAS        WSVAAS
036000960117     C                   Z-ADD     ARBLNP        WSVLNP
036100960117     C                   Z-ADD     ARBNRS        WSVNRS
036200960117     C                   Z-ADD     ARBNSP        WSVNSP
036300120706      *
036400120706      *  Decodifico la Linea di Partenza a rottura della stessa
036500120706     C                   if        *in05
036600120920     C*//                Z-ADD     ARBLNP        STCLNP
036700120920     C*//  ARBlnp        CHAIN     AZORG01L
036800120920     c                   eval      STClnpX = dARBflrX.ARBlnpX
036900120920     c                   eval      ORGfil  = %int(dARBflrX.ARBlnpX)
037000120920     C     ORGfil        chain     AZORG01L
037100120706     C                   if        %found(AZORG01L)
037200120706     C                   MOVEL     ORGdes        STDlnp
037300120706     C                   else
037400120706     C                   clear                   STDlnp
037500120706     C                   endif
037600120911      *//* - Verifica se anche la LNP orig. è abilitata alla scannerizzazione
037700120911      *//*   (in tab. "WS")
037800120911     C*//                select
037900120911     C*//                when      not  $in07parz
038000120911     C*//                eval      *in07 = $in07parz
038100120911     C*//                when      $in07parz  and  D80sns = 'S'
038200120911     C*//                eval      *in07 = $in07parz
038300120911     C*//                when      $in07parz  and  D80sns = 'N'
038400120911     C*//                eval      Kkut = 1
038500120911     C*//                eval      Kcod = 'WS'
038600120911     C*//                eval      Kkey = dARBflrX.ARBlnpX
038700120911     C*//  KTAB1         chain     TABEL
038800120911     C*//                eval      *in07 = (%found(TABEL00F)  and
038900120911     C*//                                   TBLflg = *blank)
039000120911     C*//                endsl
039100120911      * - Considera ora TUTTE le filiali abilitate alla scannerizzazione
039200120911     c                   eval      *in07 = *on
039300120706     C                   endif
039400120706      *
039500960117      *  Mi posiziono sull'archivio delle bolle in arrivo per
039600960117      *  effettuare stampa
039700960117     C                   Z-ADD     ARBAAS        KAAS
039800960117     C                   Z-ADD     ARBLNP        KLNP
039900960117     C                   Z-ADD     ARBNRS        KNRS
040000960117     C                   Z-ADD     ARBNSP        KNSP
040100120625     C     KARB          CHAIN     FNARB000                           31
040200120625if  1C     *IN31         IFEQ      '0'
040300960117      *--------------------------------------------------------------*
040400960117      *  Se ristampa considero bolla sempre
040500120625if  2C     D80RIS        IFEQ      'S'
040600991207      *  stampa solo se è stata CONSEGNATA
040700991207     C     ARBDCM        ORNE      0
040800040303      *
040900040319      *  Se deve fare elenco merce da distruggere
041000120625if  3c                   if        *in99 = *on
041100040319      *  98 on  = GD
041200040319      *  98 off = GS
041300040319     c                   seton                                        98
041400040319      *
041500040319     C                   MOVEL     ARBGgA        STCGMA
041600040319     C                   Z-ADD     1             X
041700040319     C     arbGGA        LOOKUP    C7Q(X)                                 32
041800120625if  4c                   if        *in32 = *on
041900040319     c                   movel     d7W(X)        ds7w
042000120625if  5c                   if        §7W7CA1 = 'S' or
042100040319     c                             §7W7CA2 = 'S' or
042200040319     c                             §7W7CA3 = 'S'
042300040319      * variabilizzato il chiodo "GD"
042400040319     c                   setoff                                       98
042500120706e   5c                   endif
042600040319      *
042700040319      *  record da non utilizzare x distruzione
042800040319     c                   setoff                                       32
042900040319      * se da distruggere
043000120625if  5c                   if        §7WCA1 = 'S' or
043100040319     c                             §7WCA2 = 'S' or
043200040319     c                             §7WCA3 = 'S' or
043300040319     c                             §7WCA4 = 'S' or
043400040319     c                             §7WCA5 = 'S' or
043500040319     c                             §7W7CA1 = 'S' or
043600040319     c                             §7W7CA2 = 'S' or
043700040319     c                             §7W7CA3 = 'S'
043800040319     c                   seton                                        32
043900120706e   5c                   endif
044000120706e   4c                   endif
044100040319      *
044200120625x   3c                   else
044300120706      *
044400120706      * Se Lista invio alla Partenza
044500040319     C                   Z-ADD     1             X
044600040319     C     ARBGMA        LOOKUP    C7R(X)                                 32
044700120709      *
044800120713      *
044900120727     c                   eval      $CallTNSB25 = *off
045000120727      *
045100120727      * - Se FGS abilitata alla scannerizzazione:
045200120727      *   verifica se cliente abilitato e se documenti già scannerizzati
045300120911      *  ?SE NON "Elenco sped. con obbligo restit. doc. alla LNP"?
045400120727      *     ?(vedi *in07)?
045500120727      *  ?=> NON va stampata l'indicazione "SCANNERIZZATI"/"DA SCANNERIZZ."?
045600120727      *     ?(quindi inutile controllare l'abilitazione del mittente)?
045700120727if  4c                   if        *in32  and  *in07
045800120727      *
045900120713      * - Reperimento tab. "JDC" del cliente
046000120713     c                   exsr      sr_RepTabJDC
046100120727      * - Verifica se documenti da_scannerizzare/scannerizzati
046200120713     c                   clear                   dAR5doc
046300120713if  5C                   if        D80sns      = 'N'   or
046400120713     C                             $CallTNSB25 = *on
046500120709     C                   eval      KGEN = 'DOC'
046600120712     C     K501          chain     FIAR531C
046700120713if  6c                   if        %found(FIAR531C)
046800120709     c                   movel     AR5uni        dAR5doc
046900120713e   6c                   endif
047000120713e   5C                   endif
047100120727      * - Se richieste (in ristampa) le sole spedizioni con documenti
047200120712      *   NON ancora scannerizzati: seleziono solo quelli da scannerizzare
047300120713if  5C                   if        D80sns       = 'S'   and
047400120712     C                             ($CallTNSB25 = *off   or
047500120712     C                              dAR5doc.§AR5jfid = 'S')
047600120709     C                   eval      *in32 = *off
047700120709e   5C                   endif
047800120713      *
047900120709e   4C                   endif
048000120709      *
048100120706e   3c                   endif
048200120727      *
048300991130     C*
048400120625if  3C     *IN32         IFEQ      *ON
048500991130     C     D80DPD        ANDEQ     'N'
048600991130     C     T7R(X)        ANDNE     'D'
048700991130     C     *IN32         OREQ      *ON
048800991130     C     D80DPD        ANDEQ     'S'
048900991130     C     T7R(X)        ANDEQ     'D'
049000120706     C     *IN32         OREQ      *ON
049100021029     C     D80DPD        ANDEQ     'X'
049200120706     C                   exsr      StpBol2
049300120709e   3C                   ENDIF
049400120712if  3c                   if        *in32          and
049500120712     c                             D80dpd =  'N'  and
049600120712     c                             T7R(X) <> 'D'  and
049700120712     c                             WSTAMP =  'S'  and
049800120712     c                             $CallTNSB25      = *on   and
049900120712     c                             dAR5doc.§AR5jfid = *blank
050000120709     c                   exsr      sr_CallTNSB25
050100120712e   3c                   endif
050200960117      *
050300120706e   2C                   ENDIF                                                  bolla OK
050400120706e   1C                   ENDIF                                                  trova bolla
050500960117      *
050600991207      *  Se sono in stampa aggiorno FILE SPIA
050700120625if  1C     D80RIS        IFNE      'S'
050800960117     C     *IN31         ANDEQ     '0'
050900991222      *  Aggiorna solo se è stata STAMPATA
051000120625if  2C     WSTAMP        IFEQ      'S'
051100960117     C                   Z-ADD     WOGGI         ARBDST
051200120625if  3C     D80DPD        IFEQ      'S'
051300000221     C                   EXCEPT    AGGDA3
051400120625x   3C                   ELSE
051500000221     C                   EXCEPT    AGGDAT
051600120706e   3C                   ENDIF                                                  D.P.D
051700120706e   2C                   ENDIF                                                  BOLLA OK
051800120706e   1C                   ENDIF                                                  stampa
051900960117      *
052000960117     C                   ENDSR
052100120706      *--------------------------------------------------------------*
052200120709      *  STPBOL2: Stampa bolla in elenco                             *
052300120706      *--------------------------------------------------------------*
052400120706     C     STPBOL2       BEGSR
052500120706      *
052600120706      *  Giro la data distinta di consengna in base alla bolla
052700120706      *  solo se non l'ho già impostata
052800120706if  1C     STCDDC        IFEQ      0
052900120706     C                   Z-ADD     ARBDDC        G02INV
053000120706     C                   MOVEL     '3'           G02ERR
053100120706     C                   CALL      'XSRDA8'
053200120706     C                   PARM                    WLBDA8
053300120706     C                   Z-ADD     G02DAT        STCDDC            8 0
053400120706     C                   Z-ADD     ARBNDC        STCNDC
053500120706e   1C                   ENDIF
053600120706      *  Giro la data spedizione
053700120706     C                   MOVEL     ARBMGS        WMM               2 0
053800120706     C                   MOVE      ARBMGS        WGG               2 0
053900120706     C                   MOVE      WMM           STCMGS
054000120706     C                   MOVEL     WGG           STCMGS
054100120706      *  Giro i dati
054200120706     C                   Z-ADD     ARBLNA        STCLNA
054300120706     C                   Z-ADD     ARBAAS        STCAAS
054400120920     C                   Z-ADD     ARBLNP        STCLNP
054500120706     C                   Z-ADD     ARBNRS        STCNRS
054600120706     C                   Z-ADD     ARBNSP        STCNSP
054700120706     C                   Z-ADD     ARBRMN        STCRMN
054800120706      *  Deve segnalare la Zona di consegna al posto del cod.Bolla
054900120706     C                   MOVEL     ARBZNC        STCZNC
055000120706     C                   MOVEL     ARBRSM        STCRSM
055100120706     C                   MOVEL     ARBRSD        STCRSD
055200120706     C                   MOVEL     ARBRMA        STCRMA
055300120706     C                   MOVEL     ARBGMA        STCGMA
055400120706      *  Impostazione indicazione della scannerizzazione
055500120712     c                   clear                   STscann
055600120727     c                   eval      *in09 = *off
055700120712if  1c                   if        $CallTNSB25
055800120712if  2C                   if        dAR5doc.§AR5jfid = *blank
055900120709     C                   eval      STscann = 'DA SCANNERIZZ. '
056000120727     c                   eval      *in09 = *on
056100120712x   2C                   else
056200120713     C                   eval      STscann = 'SCANNERIZZATI  '
056300120712e   2C                   endif
056400120712e   1c                   endif
056500120706      *
056600120706     C                   SETOFF                                       0203
056700120706if  1C     T7R(X)        IFEQ      'B'
056800120706     C                   SETON                                            02
056900120706x   1C                   ELSE
057000120706     C                   SETON                                            03
057100120706e   1C                   ENDIF
057200120706     C* Se si è rotta la chiave
057300120706     C* e la stampa non è per DPD
057400120709     C* e NON è stata richiesta la RIstampa delle sole spedizioni
057500120709     C*   con documenti da restituire e NON ancora scannerizzati
057600120706     C* salto pagina.
057700120706if  1C     *IN05         IFEQ      *ON
057800120706     C     D80DPD        ANDEQ     'N'
057900120709     C     D80sns        andne     'S'
058000120706     C     *IN05         orEQ      *ON
058100120706     C     D80DPD        ANDEQ     'X'
058200120706     C                   SETON                                        04
058300120706e   1C                   ENDIF
058400120706     C* Se si è rotta la data distinta x bolle DPD
058500120706     C* salto pagina.
058600120706if  1C     *IN06         IFEQ      *ON
058700120706     C     D80DPD        ANDEQ     'S'
058800120706if  2C     STTOTB        IFNE      0
058900120706     C                   WRITE     TOT01
059000120706e   2C                   ENDIF
059100120706     C                   SETON                                        04
059200120706e   1C                   ENDIF
059300120706      *
059400120706     C* Salto pagina.
059500120706     c                   exsr      Se_OVERFLW
059600120706      *
059700120706     C                   WRITE     DET01
059800120706     C                   ADD       1             STTOTB
059900120706      *
060000120706      * Se I.M.G. quindi con codice 'GS' 98=(off) deve stampare su altra riga
060100120706      *    le informazioni di giacenza e ubicazione.
060200120706if  1c                   IF        *in99 = *on
060300120706      *
060400120706     c                   clear                   stcubi
060500120706     c                   clear                   stcfgc
060600120706     c                   clear                   stcngc
060700120706      *
060800120706      * Solo x I.M.G. (Giacenza e ubicazione)
060900120706if  2c                   if        *in98 = *off
061000120706     C     Kgca          CHAIN     tigcp000
061100120706if  3c                   if        %Found(tigcp21L)
061200120706     c                   eval      stcfgc = gcpfgc
061300120706     c                   eval      stcngc = gcpngc
061400120706      * reperisce l'ubicazione se c'è della giacenza
061500120706     C                   move      'GEN'         KGEN
061600120706     C     K501          Chain     Fiar5000
061700120706if  4c                   if        %Found(Fiar501L)
061800120706     c                   movel     ar5uni        dar5GEN
061900120706      * Con la DS "GEN" reperisce l'ubicazione
062000120706if  5c                   if        §AR5ZNG <> *blank
062100120706     c                   eval      stcubi = §AR5ZNG
062200120706e   5c                   endif
062300120706e   4c                   endif
062400120706e   3c                   endif
062500120706e   2c                   endif
062600120706      *
062700120706     C* Stampa una seconda riga per indicare i colli da distruggere
062800120706      *    dettaglio segnacolli
062900120706     C                   clear                   NCL
063000120706     c                   z-add     0             NC                3 0
063100120706     c                   seton                                        34
063200120706      *
063300120706     C     KARB          setll     Fnart000
063400120706     C     KARB          reade     Fnart000                               35
063500120706      *
063600120706      * Se per qualche strano motivo non ci fosse ART deve comunque stampare
063700120706      *  la giacenza e ubicazione (NON ACCADRA' MAI)
063800120706if  2c                   if        %EoF(Fnart01L)
063900120706     c                   exsr      Se_OVERFLW
064000120706     C                   WRITE     DETcol
064100120706e   2c                   endif
064200120706      *
064300120706do  2c                   dow       not %EoF(Fnart01L)
064400120706      *
064500120706      * Memorizza un segnacollo in una posizione di schiera
064600120706     c                   add       1             NC
064700120706     c                   z-add     artNSC        NCL(nc)
064800120706     C     KARB          reade     Fnart000                               35
064900120706      *
065000120706      *  Se è arrivato al limite di schiera (della riga da stampare)
065100120706      *  o se si trova con dei segnacollli impostati da stampare ma è
065200120706      *  arrivato a EoF quindi uscirebbe dal ciclo --> EMETTE LA RIGA.
065300120706if  3c                   if        nc = 15 or %EoF(Fnart01L)
065400120706     c                   exsr      Se_OVERFLW
065500120706     C                   WRITE     DETcol
065600120706     c                   setoFF                                       34
065700120706     C                   clear                   NCL
065800120706     c                   z-add     0             NC
065900120706e   3c                   endif
066000120706      *
066100120706e   2c                   enddo
066200120706      *
066300120706e   1c                   ENDIF
066400120706      *
066500120706     C                   MOVEL     'S'           WSTAMP            1            SI STAMPA
066600120706      *
066700120706     C                   ENDSR
066800120706      *
066900960117      *--------------------------------------------------------------*
067000040303      *  Salto Pagina imposta la testata                             *
067100120709      *  Rottura LNP in RIstampa spedizioni con restituzione docum.  *
067200120709      *    alla Partenza: stampa intestazione LNP                    *
067300960117      *--------------------------------------------------------------*
067400040303     C     Se_OVERFLW    BEGSR
067500040303      *
067600040303     C* Salto pagina.
067700040303     C     *IN04         IFEQ      *ON
067800120709      *
067900040303     C                   SETOFF                                       04
068000040303     C                   WRITE     TES01
068100120709     C                   WRITE     TES02
068200040303      *
068300040303     C* Se si è rotta la chiave  e la stampa non è per DPD
068400120709     C*   e  non è una RIstampa delle spedizioni con documenti da
068500120709     C*      restituire alla partenza NON ancora scannerizzati
068600040303     C* stampo i dati di testata.
068700120730     c                   if        D80ris = 'N'  or  D80sns = 'N'
068800120709     c                   write     Tes03
068900120709     c                   endif
069000120709     c                   write     Tes04
069100120730     c                   if        D80ris = 'S'  and
069200120730     c                             D80sns = 'S'  and
069300120709     c                             D80dpd = 'N'  and
069400120709     c                             *in05  = *on
069500120709     c                   write     Tes03
069600120709     c                   endif
069700120709      *
069800040303     C   05              IF        D80DPD = 'N' or D80DPD = 'X'
069900040303     C                   SETOFF                                       05
070000040303     C                   ENDIF
070100040303     C   06              IF        D80DPD = 'S'
070200040303     C                   SETOFF                                       06
070300040303     C                   Z-ADD     0             STTOTB
070400040303     C                   ENDIF
070500120709      *
070600040303     C                   ENDIF
070700120709      *
070800120709      * Sola intestazione LNP
070900120709     c                   if        *in05  = *on  and
071000120709     c                             D80dpd = 'N'
071100120709     c                   eval      *in05  = *off
071200120730     c                   if        D80ris = 'S'  and  D80sns = 'S'
071300120709     c                   write     TES03
071400120709     c                   else
071500120709     c                   write     TES01
071600120709     c                   write     TES02
071700120709     c                   write     TES03
071800120709     c                   write     TES04
071900120709     c                   endif
072000120709     c                   endif
072100040303      *
072200040303     C                   ENDSR
072300120625      /free
072400120625
072500120625       //-------------------------------------------------------------
072600120625       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
072700120625       //-------------------------------------------------------------
072800120712       BEGSR  sr_RepTabJDC;
072900120712
073000120712         $CallTNSB25 = *off;
073100120625
073200120625         // -?Aggancio tabella "JDC" per reperimento data attivazione?
073300120625         //  ?della scannerizzazione documenti cliente?
073400120625         clear  dJDC;
073500120710         clear  k05tntbe01;
073600120710         k_TBEcod = 'JDC';
073700120702         // -?ARBCCM se valorizzato, ARBKSC altrimenti?
073800120702         if  ARBccm <> *zero;
073900120710           k_TBEke1 = %editc( ARBccm : 'X' );
074000120702         else;
074100120710           k_TBEke1 = %editc( ARBksc : 'X' );
074200120702         endif;
074300120710
074400120710         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
074500120710
074600120917         if  %found(TNTBE01L)  and  TBEatb = *blank;
074700120710           dJDC = TBEuni;
074800120710         endif;
074900120625
075000120625         // -?Scannerizzazione documenti per il cliente: NON prevista?
075100120625         //  ?o non ancora in vigore o già scaduta?
075200120710         if  NOT  %found(TNTBE01L)                or
075300120917             TBEatb <> *blank                     or
075400120710             §JDCdti > (ARBaas * 10000) + ARBmgs  or
075500120625             §JDCdtf < (ARBaas * 10000) + ARBmgs;
075600120625           leavesr;
075700120625         endif;
075800120712
075900120712         $CallTNSB25 = *on;
076000120712
076100120712       ENDSR;
076200120625
076300120712       //-------------------------------------------------------------
076400120712       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
076500120712       //-------------------------------------------------------------
076600120712       BEGSR  sr_CallTNSB25;
076700120712
076800120625         reset  TNSB25ds;
076900120625         //D25ges = 'T'       ?(già così)?
077000120625         D25aas = ARBaas;
077100120625         D25lnp = ARBlnp;
077200120625         D25nrs = ARBnrs;
077300120625         D25nsp = ARBnsp;
077400120709         if  wCount >= §VPOpagLdV;
077500120625           D25fl1 = *on;
077600120625           clear  wCount;
077700120625         endif;
077800120625
077900120625         tnsb25r ( TNSB25ds );
078000120625
078100120625         wCount += 1;
078200120625
078300120625       ENDSR;
078400120625
078500120625      /end-free
078600040303      *--------------------------------------------------------------*
078700040303      *  Operazioni iniziali                                         *
078800040303      *--------------------------------------------------------------*
078900960117     C     *INZSR        BEGSR
079000960117      *
079100960117     C     *ENTRY        PLIST
079200960117     C                   PARM                    KPJBA
079300120706     C                   MOVEL     KPJBU         FNLR80DS
079400120710      *
079500120710      /free
079600120710
079700120710         // -?Verifica del sistema AS/400 corrente?
079800120710         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
079900120710           *inLR = *on;
080000120710           return;
080100120710         endif;
080200120710
080300120710         // -?Reperimento dati job?
080400120710         in(E) §AzUte;
080500120710         if NOT %error;
080600120710           in(E) §DatiUte;
080700120710         endif;
080800120710         if %error or RSut = *blanks;
080900120710           clear TIBS34ds;
081000120710           tibs34r ( tibs34ds );
081100120710           in §AzUte;
081200120710           in §DatiUte;
081300120710         endif;
081400120710
081500120710         // -?Apertura file TNTBE01L?
081600120710         if  %subst( currSysNeta : 1 : 6 ) = 'SETRAS';
081700120710           wLibTNTBE = 'GAITRAGRU/TNTBE01L';
081800120712           wLibFIAR5 = 'GAITRAGRU/FIAR531C';
081900120710         else;
082000120710           wLibTNTBE = 'GAITRAGRPS/TNTBE01L';
082100120712           wLibFIAR5 = 'GAITRAGRPS/FIAR531C';
082200120710         endif;
082300120710         open  TNTBE01L;
082400120712         open  FIAR531C;
082500120710
082600120710      /end-free
082700040308     C*
082800120710     C* imposta la data del giorno
082900120710     c                   move      *date         udate8
083000991130     C*
083100991130     C     D80DPD        IFEQ      'S'
083200991130     C                   MOVEL     CPERDP        STTITO
083300991130     C                   ENDIF
083400040303      *
083500120709     C                   eval      *in07 = (D80dpd = 'N')
083600120911     C*//                if        *in07
083700120911     C*//                eval      Kkut = 1
083800120911     C*//                eval      Kcod = 'WS'
083900120911     C*//                eval      Kkey = %editc( D80fgs : 'X' )
084000120911     C*//  KTAB1         chain     TABEL
084100120911     C*//                eval      *in07 = (%found(TABEL00F)  and
084200120911     C*//                                   TBLflg = *blank)
084300120911     C*//                endif
084400120911     C*//                eval      $in07parz = *in07
084500120727     C                   eval      *in08 = (*in07  and  D80sns = 'S')
084600021029     C     D80DPD        COMP      'X'                                    99
084700960117      *----------------------------------------------*
084800960117      *  Definizione chiavi di accesso
084900960117     C     KARB          KLIST
085000960117     C                   KFLD                    KAAS
085100960117     C                   KFLD                    KLNP
085200960117     C                   KFLD                    KNRS
085300960117     C                   KFLD                    KNSP
085400040316      *
085500040316     C     KLBL          KLIST
085600040316     C                   KFLD                    ARBAAS
085700040316     C                   KFLD                    ARBLNP
085800040316     C                   KFLD                    ARBNRS
085900040316     C                   KFLD                    ARBNSP
086000040316      *
086100960117     C     KARBX         KLIST
086200960117     C                   KFLD                    KFGS
086300960117     C                   KFLD                    KDST
086400960117     C     KTAB          KLIST
086500960117     C                   KFLD                    KKUT
086600960117     C                   KFLD                    KCOD
086700040319     C     KTAB1         KLIST
086800040319     C                   KFLD                    KKUT
086900040319     C                   KFLD                    KCOD
087000040319     C                   KFLD                    Kkey
087100040303     C     KGCA          KLIST
087200040303     C                   KFLD                    KAAS
087300040303     C                   KFLD                    KLNP
087400040303     C                   KFLD                    KNRS
087500040303     C                   KFLD                    KNSP
087600040303     C                   KFLD                    KZero
087700040303     C                   z-add     0             KZero
087800040304     C     K501          KLIST
087900040303     C                   KFLD                    KAAS
088000040303     C                   KFLD                    KLNP
088100040303     C                   KFLD                    KNRS
088200040303     C                   KFLD                    KNSP
088300040303     C                   KFLD                    KGEN              3
088400040303     C                   move      'GEN'         KGEN
088500960117      *----------------------------------------------*
088600960117      *  Definizione variabili
088700960117     C     *LIKE         DEFINE    ARBAAS        KAAS
088800960117     C     *LIKE         DEFINE    ARBLNP        KLNP
088900960117     C     *LIKE         DEFINE    ARBNRS        KNRS
089000960117     C     *LIKE         DEFINE    ARBNSP        KNSP
089100960117     C     *LIKE         DEFINE    ARBFGS        KFGS
089200960117     C     *LIKE         DEFINE    ARBDST        KDST
089300960117     C     *LIKE         DEFINE    TBLKUT        KKUT
089400960117     C     *LIKE         DEFINE    TBLCOD        KCOD
089500040319     C     *LIKE         DEFINE    TBLkey        Kkey
089600960117     C     *LIKE         DEFINE    ARBAAS        WSVAAS
089700960117     C     *LIKE         DEFINE    ARBLNP        WSVLNP
089800960117     C     *LIKE         DEFINE    ARBNRS        WSVNRS
089900960117     C     *LIKE         DEFINE    ARBNSP        WSVNSP
090000040304     C     *like         define    gcpfrg        KZero
090100960117      *----------------------------------------------*
090200960117     C                   CLEAR                   WLBDA8
090300960117     C     D80RIS        IFNE      'S'
090400960117      *  Calcolo la data del giorno
090500960117     C                   SETON                                        01
090600960117     C                   TIME                    WHHDAT           14 0
090700960117     C                   CLEAR                   WLBDA8
090800960117     C                   Z-ADD     WHHDAT        G02DAT
090900960117     C                   CALL      'XSRDA8'
091000960117     C                   PARM                    WLBDA8
091100960117     C                   Z-ADD     G02INV        WOGGI             8 0
091200960117     C                   Z-ADD     G02INV        WDTSTA            8 0
091300960117     C                   ELSE
091400960117     C                   Z-ADD     D80DST        WDTSTA
091500960117     C                   END
091600960117      *  Giro data stampa/ristampa
091700960117     C                   Z-ADD     WDTSTA        G02INV
091800960117     C                   MOVEL     '3'           G02ERR
091900960117     C                   CALL      'XSRDA8'
092000960117     C                   PARM                    WLBDA8
092100960118     C                   Z-ADD     G02DAT        STCDTS
092200960117      *----------------------------------------------*
092300960117      *  Eseguo caricamento tab.7R
092400960117     C                   Z-ADD     0             X                 3 0
092500960117     C                   Z-ADD     1             KKUT
092600960117     C                   MOVEL     '7R'          KCOD
092700960117     C     KTAB          CHAIN     TABEL00F                           96
092800960117     C     *IN96         DOWEQ     '0'
092900960117     C     X             ANDLT     100
093000960117     C                   MOVEL     TBLUNI        DS7R
093100960117     C     TBLFLG        IFEQ      ' '
093200960117     C     §7R1BF        ANDNE     *BLANKS
093300960117     C                   ADD       1             X
093400960117     C                   MOVEL     TBLKEY        C7R(X)
093500960117     C                   MOVEL     §7R1BF        T7R(X)
093600960117     C                   END
093700960117     C     KTAB          READE     TABEL00F                               96
093800960117     C                   END
093900960117     C     *IN96         IFEQ      '0'
094000960117     C                   SETON                                        H1
094100960117     C                   GOTO      FINE
094200960117     C                   END
094300960117      *----------------------------------------------*
094400040319      *  Eseguo caricamento tab.7Q
094500040319     c                   clear                   c7Q
094600040319     c                   clear                   d7Q
094700040319     c                   clear                   d7W
094800040319      *
094900040319     C                   Z-ADD     0             X                 3 0
095000040319     C                   Z-ADD     1             KKUT
095100040319     C                   MOVEL     '7Q'          KCOD
095200040319     C     KTAB          CHAIN     TABEL00F                           96
095300040319     C                   Dow       *IN96 = *off and X < 100
095400040319     C                   if        tblFLG = *blank
095500040319     C                   add       1             X
095600040319     C                   MOVEL     TBLKEY        C7Q(X)
095700040319     C                   MOVEL     TBLUNI        D7Q(X)
095800040319     C                   end
095900040319     C     KTAB          READE     TABEL00F                               96
096000040319     C                   End
096100040319      *
096200040319      * carica il proseguimento della tabella
096300040319      *  Eseguo caricamento tab.7W
096400040319     c                   do        100           X
096500040319     c                   if        c7q(X) = *blank
096600040319     c                   leave
096700040319     c                   end
096800040319      *
096900040319      * carica il proseguimento della tabella se c'è
097000040319     C                   Z-ADD     1             KKUT
097100040319     C                   MOVEL     '7W'          KCOD
097200040319     C                   movel(p)  c7q(x)        Kkey
097300040319     C     KTAB1         CHAIN     TABEL00F                           96
097400040319     C                   clear                   D7W(X)
097500040319     C  n96              MOVEL     TBLUNI        D7W(X)
097600040319     c                   endDO
097700120625      /free
097800120625
097900120625         // -?Inschieramento codici bolla con porto franco da tab. "3A"?
098000120626         clear  $3A;
098100120626         clear  x;
098200120625         kkut = 1;
098300120625         kcod = '3A';
098400120625         setll  (kkut : kcod)  TABEL;
098500120625         reade  (kkut : kcod)  TABEL;
098600120625         DoW  Not %eof(TABEL00F);
098700120625           ds3A = TBLuni;
098800120625           if  TBLflg = *blank  and  %subst( §3Atb1 : 1 : 1 ) = 'F';
098900120625             x += 1;
099000120625             $3A(x) = TBLkey;
099100120625           endif;
099200120625           reade  (kkut : kcod)  TABEL;
099300120625         EndDo;
099400120625
099500120625         // -?Aggancio tabella "VPO" per reperimento?
099600120625         //  ?n° pagine limite in stampa LdV (per close/open PrtF)?
099700120625         clear  dVPO;
099800120710         clear  k05tntbe01;
099900120710         k_TBEcod = 'VPO';
100000120710         k_TBEke1 = 'VPO';
100100120710         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
100200120710         if  %found(TNTBE01L);
100300120710           dVPO = TBEuni;
100400120625         else;
100500120625           §VPOpagLdV = 150;
100600120625         endif;
100700120625
100800120625      /end-free
100900040319      *----------------------------------------------*
101000960117      *  Reperisco filiale
101100960117     C                   Z-ADD     0             STCFIL
101200120710     C     DUTlpo        IFEQ      '2'
101300120710     C     DUTlpo        OREQ      *BLANKS
101400960117     C                   SETOFF                                       05
101500120710     C                   MOVE      DUTpou        STCFIL
101600960117     C                   ELSE
101700960117     C                   SETON                                        05
101800960117     C                   MOVEL     SIMFEL        STCFIL
101900960117     C                   END
102000000225      *  Accendo indicatore 06
102100000225     C                   SETON                                        06
102200960117      *  Decodifico filiale
102300960117     C                   Z-ADD     STCFIL        ORGFIL
102400960117     C                   MOVEL     *BLANKS       ORGDES
102500960117     C     ORGFIL        CHAIN     AZORG01L                           30
102600960117     C                   MOVEL     ORGDES        STDFIL
102700960117      *  Reperisco filiale gestione
102800960117     C                   MOVEL     D80FGS        STCFGS
102900960117     C                   Z-ADD     STCFGS        ORGFIL
103000960117     C                   MOVEL     *BLANKS       ORGDES
103100960117     C     ORGFIL        CHAIN     AZORG01L                           30
103200960117     C                   MOVEL     ORGDES        STDFGS
103300040305     C                   MOVEL     ORGDE6        OG146
103400960117      *  Altri dati
103500960117     C                   MOVEL     NOMPGM        STCPGM
103600960117     C                   MOVEL     *ALL'-'       CMP198
103700960117     C                   MOVEL     *ALL'-'       CM1198
103800991130     C                   SETON                                        04
103900000221      *
104000000221      * Se elaborazione DPD gestisco  apertura vista
104100000221      * logica per data e nr. distinta
104200000221     C     D80DPD        IFEQ      'S'
104300000221     C                   OPEN      FNARBX3L
104400000221      * .. altrimenti gestisco apertiura vista logica
104500000221      * x numero spedizione
104600000221     C                   ELSE
104700120920     C                   OPEN      FNARBX4L
104800000221     C                   END
104900960117      *----------------------------------------------*
105000960117     C                   ENDSR
105100960117      *----------------------------------------------*
105200960117     OFNARBX00  E            AGGDAT
105300960117     O                       ARBDST
105400000221     OFNARBX3   E            AGGDA3
105500000221     O                       ARBDST
