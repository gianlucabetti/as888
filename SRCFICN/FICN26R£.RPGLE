000100030512     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300020308     Ffifgt03l  uf   e           k disk
000400020313     Ffifgt01l  if   e           k disk    rename(fifgt000:fifgt1)
000500030207     Ffifpt01l  if   e           k disk
000600021203     Ffiapd01l  if   e           k disk
000700101105     F*****Ansif01l  if   e           k disk
000800020305     Ftabel00f  if   e           k disk
000900050404     fazcae03l  if   e           k disk    usropn
001000030206     FAzOrg01L  if   e           k disk
001100030206     FAnRco98j  if   e           k disk
001200030207     FFICN26P   O    e             PRINTER oflind(*in01)
001300030227      *****************************************************************
001400030227      *  Riepilogo Indicatori utilizzati per la Stampa
001500030227      *  01 = (on) Overflow
001600030227      *  21 = Generico
001700030227      *  44 = Stampa x Tariffe Pacchi Poste
001800030227      *  99 = Generico
001900030227      *****************************************************************
002000030206     D C8a             S              3    DIM(99)                              Codice Tariffa
002100030207     D Tpg             S              1    DIM(99)                              Tipo Pagam Tariffa
002101130619     D esc             S              1    DIM(99)                              Tipo Pagam Tariffa
002200030206     D E12             S              1    DIM(99)                              1/2/E lvl filiali
002500030206     D Des             S             70    DIM(99)                              Descrizione 8F
002600030206     D Or1             S              2    DIM(99)                              Ordina 1    8F
002700030206     D Or2             S              2    DIM(99)                              Ordina 2    8F
002800030206     D Or3             S              2    DIM(99)                              Ordina 3    8F
002900030206     D Ord             S             10    DIM(99)                              Ordinamento 8F
003000011026
003100030206     D Tibs36ds      E DS
003200030206     D ut§dse        E DS                  extname(ut§dse0f)
003300030520     D og143         E DS
003400050201     D og150         E DS
003500030206     D CNCR80        E DS
003600030213     D CNCR41        E DS
003700030213     D CNCR42        E DS
003800030207     d fnlv55ds      e ds
003900030227     dkpjba          e ds
004000020305     dds8a           e ds
004100030206     dds8f           e ds
004200101105     ***************************************************************************
004300101105     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
004400101105     D ESITO_OK...
004500101105     D                 C                   0
004600101105     ***************************************************************************
004700101105     **
004800101105     ** Dichiarazione prototipi procedure esterne.
004900101105     **
005000101105     ***************************************************************************
005100101105      /DEFINE DFTACTGRP_YES
005200101105     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
005300101105      /UNDEFINE DFTACTGRP_YES
005400101105     ***************************************************************************
005500101105     **
005600101105     ** Definizione strutture dati.
005700101105     **
005800101105      **************************************************************************
005900101105     D tibsSocI0...
006000101105     D               E DS                  QUALIFIED
006100101105     D                                     INZ
006200101105     D tibsSocO0...
006300101105     D               E DS                  QUALIFIED
006400101105     D                                     INZ
006500101105     ***************************************************************************
006600101105     **
006700101105     ** Definizione variabili modulo/programma.
006800101105     **
006900101105     ***************************************************************************
007000101105     D prmRqsOpCode...
007100101105     D                 S             10A
007200101105     D prmRpyOpCode...
007300101105     D                 S             10A
007400101105     D prmRpyIdMsg...
007500101105     D                 S             10I 0
007600101105     D prmRqsFormato...
007700101105     D                 S             10A
007800101105     D prmRqsDataSize...
007900101105     D                 S             10I 0
008000101105     D prmRpyFormato...
008100101105     D                 S             10A
008200101105     D prmRpyDataSize...
008300101105     D                 S             10I 0
008400101105
008500101105     ***************************************************************************
008600030226      *
008700030226     D PARAM3          DS
008800030226     D  NUMER                  1     12  0
008900030226     D  LETTER                13    122
009000030226     D  DECIM                123    124  0
009100030226     D  BYTE                 125    125
009200030226      *
009300030226     D ALF             S              1    DIM(60)
009400030226     D INTERO          S              9  0 inz
009500030226     D CENTO           S              3    inz
009600030227     D SavLET          S             60    inz
009700030226     D XJ              S              3  0 inz
009800030226     D DECNUM          S              3    inz
009900030226     D SALNUM          S             11  2 inz
010000030227     d savkpj          s                   like(kpjbu)
010100030227     D numa            S              1    DIM(10)
010200030227     D numa1           S              1    DIM(11)
010300030227     D pul65           S              1    DIM(65)
010400030227     D pul21           S              1    DIM(21)
010500030226      *
010600020304     dparam            ds
010700020308     D lancio                         1
010800020304     d codpadr                        7s 0
010900020308     D codpadr2                       7s 0
011000020304     d alladat                        8s 0
011100030206     D  tipserv                       1
011200030227     D  autconv                       1
011300030206
011400030206     d DS10fld         ds
011500030206     D  Ord_01                        2
011600030206     D  Ord_02                        2
011700030206     D  Ord_03                        2
011800030206     D  Cod_04                        3
011900030206     D  Lvl_05                        1
012000020304
012100030207     D CntRig          S              3  0
012200030207     D UteSed          S              1
012300030206     d sav_x8a         s                   like(x8a)
012400030206     d sav_01          s                   like(Ord_01)
012500030206     d sav_02          s                   like(Ord_02)
012600030206
012700020305     d dataiso         s               d   datfmt(*iso)
012800020305     d dataeur         s               d   datfmt(*eur)
012900030206      *
013000030206     d CORR01          C                   Const('CORRISPETTIVI CICLO PARTENZE')
013100030206     d CORR02          C                   Const('CORRISPETTIVI CICLO ARRIVI')
013200030206     d CORR03          C                   Const('CORRISPETTIVI MERCI IN TRANSI-
013300030206     d                                     TO-ARRIVI')
013400030206     d CORR04          C                   Const('CORRISPETTIVI MERCI IN TRANSI-
013500030206     d                                     TO-PARTENZE')
013600030206     d CORR05          C                   Const('CORRISPETTIVI GESTIONE GIACEN-
013700030206     d                                     ZE MAGAZZINO')
013800030206     d CORR06          C                   Const('CORRISPETTIVI CICLO INTERNAZI-
013900030206     d                                     ONALE')
014000030206     d CORR07          C                   Const('CORRISPETTIVI CICLO ARRIVI')
014100030512     d CORR08          C                   Const('CORRISPETTIVI CICLO PARTENZE')
014200030207     d MOVIM1          C                   Const('a) MOVIMENTAZIONE COLLETTAME -
014300030207     d                                     VARIO')
014400030207     d MOVIM2          C                   Const('b) MOVIMENTAZIONE "SERVIZIO P-
014500030207     d                                     ACCHI POSTA"')
014600030227     d TrattC          C                   Const('semirimorchi  e  rimorchi, la-
014700030227     d                                      somma  mensile,  forfetariamente  d-
014800030227     d                                     eterminata,  di  Euro')
014900030227     d PulizC          C                   Const('Capitolato   d''Oneri  ( cfr.-
015000030227     d                                       Allegato  A ),   l''importo  annua-
015100030227     d                                     le   forfettario  di  Euro ')
015200030227     d Pulrat          C                   Const(' che verrà suddiviso in n.12 -
015300030227     d                                     rate mensili di pari importo di -
015400030227     d                                     Euro')
015500030227     d oltiva          C                   Const(' ciascuna, oltre IVA.')
015600050125     d  CBan_sep       C                   Const('|__|__|__|__|__|__|__|__|__|-
015700050125     d                                     __|__|__|')
015800050125     d  CABI_sep       C                   Const('|__|__|__|__|__|')
015900050125     d  CCAB_sep       C                   Const('|__|__|__|__|__|   CIN |__|')
016000080208     d  CCIBAN_sep     C                   Const('|__|__|__|__|  |__|__|__|__|-
016100080208     d                                       |__|__|__|__|  |__|__|__|__|-
016200080208     d                                       |__|__|__|__|  |__|__|__|__|-
016300080211     d                                       |__|__|__|')
016400030207      *---------------------------------------------------------------*
016500030207      *  Main
016600030207      *---------------------------------------------------------------*
016700101105     ** Inizializzo il programma.
016800101105     C                   CALL      'TIBSSOCR'
016900101105     C                   PARM      'INIT'        prmRqsOpCode
017000101105     C                   PARM                    prmRpyOpCode
017100101105     C                   PARM                    prmRpyIdMsg
017200101105     C*
017300030206      *  Carica le tabelle 8A e 8F che servono per pilotare la stampa
017400030206     c                   Exsr      CarTAB_Coop
017500030206      *
017600050405      *    Controllo se la filiale è una HUB DPD
017700050404     c                   exsr      HUB_DPD
017800050404      *
017900030206      * Ciclo per Cooperative
018000020304     c                   z-add     codpadr       kpdr
018100021203     c     kapd          setll     fiapd01l
018200030207      *
018300020308     c                   do        *hival
018400030206     c     atip          reade     fiapd01l                               99
018500020308     c   99              leave
018600020308     c                   if        apdpdr > codpadr2
018700020308     c                   leave
018800020308     c                   endif
018900030224      *
019000030224      *  Dati di inizio Tariffa
019100030224     c                   Exsr      Decod_tes
019200030520      *
019300030520      *  Se è il network -> POSTA
019400030520     c                   if        §ogntw = 'PPT'
019500030520     c                   end
019600030227      *
019700030227      * Solo se trovata una tariffa valida
019800030227     c                   if        Tar_Ok ='S'
019900030224      * Cooperative
020000030224     c                   if        TipServ = 'X'
020100030206      *  per ogni anagrafica stampa la tariffa
020200020308     c                   exsr      elabora
020300030224     c                   else
020400030224      * Trattoristi/Pulizie
020500030224     c                   exsr      elabtar
020600030224     c                   end
020700030224      *
020800030224      * Dopo il dettaglio per finire il documento
020900030224     c                   exsr      fine_tar
021000030227     c                   endIF
021100030207      *
021200020308     c                   enddo
021300020304
021400101105     ** Chiudo il programma.
021500101105     C                   CALL      'TIBSSOCR'
021600101105     C                   PARM      'FINALIZE'    prmRqsOpCode
021700101105     C                   PARM                    prmRpyOpCode
021800101105     C                   PARM                    prmRpyIdMsg
021900101105     C*
022000030206     c                   seton                                        LR
022100030207     C*--------------------------------------------------------------------
022200030207      * Tabelle 8A e 8F su schiere
022300030207     C*--------------------------------------------------------------------
022400030207     c     CarTAB_Coop   begsr
022500030207      *
022600030207     C*  Codici Tariffe Coop
022700030207     C                   z-add     0             X8a               5 0
022800030207     C                   movel     '8A'          COD
022900030207     C     KTab_02       SETLL     tabel00f
023000030207     C     KTab_02       reade     tabel00f
023100030207     C*
023200030207     C                   Dow       not %Eof(tabel00f)
023300030207     C     tblFLG        ifeq      ' '
023400030207     C                   movel     tblUNI        ds8A
023500030207     C*
023600030207      * solo quelle delle Cooperative
023700030207     C*  ed il Tipo servizio è quello richiesto dal video
023800030207     C     §8aFAC        ifeq      'C'
023900030207     C     §8aUSC        andeq     TipServ
024000030207     C                   add       1             X8a
024100030207     C                   movel     tblKEY        C8a(X8a)
024200030207     C                   MOVEL     §8a12E        E12(X8a)
024300130619     C****               MOVEL     §8aFPT        PNE(X8a)
024400070212     C*********          MOVEL     §8aPA         TPA(X8a)
024500030207     C                   MOVEL     §8aTT0        Tpg(X8a)
024501130619     C                   MOVEL     §8aesccar     esc(X8a)
024600030207     C                   end
024700030207     C                   end
024800030207      *
024900030207     C     KTab_02       reade     tabel00f
025000030207     C                   EndDO
025100030207     c                   z-add     x8a           sav_x8a
025200030224     c     100           sub       x8a           sav99             5 0
025300030207      * -------
025400030207     C*  Codici Tariffe Coop tabella Correlata 8F
025500030207     c                   do        sav_x8a       x8a
025600030207      *
025700030207     C                   movel     '8F'          cod
025800030207     C                   movel     *blank        key
025900030207     C                   movel     c8a(x8a)      key
026000030207     C     KTab          chain     tabel00f
026100030207     C                   if        %Found(tabel00f)  and
026200030207     C                             tblFLG =  ' '
026300030207     C                   movel     tblUNI        ds8F
026400030207     C*
026500030207     C                   movel     §8fDES        Des(X8a)
026600050201      ***
026700030207     c                   if        §8fOr1 <> *blank
026800030207     C                   movel     §8fOr1        Or1(X8a)
026900030207     c                   else
027000030207     C                   movel     '00'          Or1(X8a)
027100030207     c                   end
027200030207     c                   if        §8fOr2 <> *blank
027300030207     C                   movel     §8fOr2        Or2(X8a)
027400030207     c                   else
027500030207     C                   movel     '00'          Or2(X8a)
027600030207     c                   end
027700030207     c                   if        §8fOr3 <> *blank
027800030207     C                   movel     §8fOr3        Or3(X8a)
027900030207     c                   else
028000030207     C                   movel     '00'          Or3(X8a)
028100030207     c                   end
028200030207     C* se non c'è la tabella
028300030207     c                   Else
028400030207     C                   movel     *blanks       Des(X8a)
028500030207     C                   movel     '00'          Or1(X8a)
028600030207     C                   movel     '00'          Or2(X8a)
028700030207     C                   movel     '00'          Or3(X8a)
028800030207     c                   End
028900030207     C*
029000030207     C* imposta il campo per riordinare la stampa in seguito
029100030207     c                   clear                   fld10            10
029200030207     c                   eval      fld10 = §8fOr1 + §8fOr2 + §8fOr3 + C8a(x8a) +
029300030207     c                             E12(x8a)
029400030207      *
029500030207     C                   movel     fld10         Ord(X8a)
029600030207      *
029700030207     c                   Enddo
029800030207      *
029900030207      *  quindi riordina la schiera di emissione di stampa
030000030207     c                   SortA     Ord
030100030207      *
030200030207     c                   endSR
030300030207     C*--------------------------------------------------------------------
030400050404      *  Controlla se è una HUB che gestisce DPD
030500050404     C*--------------------------------------------------------------------
030600050404     c     HUB_DPD       begSR
030700050404
030800050405     c                   move      'N'           Ntw_DPD_SN        1
030900050405      * Memorizza FGS a parte x controllo HUB DPD gestita dalla filiale
031000050405     c     codpadr       div       10000         fgskey            3 0
031100050405      **
031200050404     c     kcae          klist
031300050404     c                   kfld                    tipKey
031400050404     c                   kfld                    kfgs_zero
031500050405     c                   kfld                    fgskey
031600050404      **
031700050404     c                   move      'C'           tipkey            1
031800050404     c                   z-add     0             kfgs_zero         3 0
031900050404      **
032000050404     c                   open      azcae03l
032100050404      **
032200050404      *  Su AZCAE ci sono le linee gestite
032300050404     c                   setoff                                       92
032400050404     c     kcae          setll     azcae03l
032500050404     c     kcae          reade     azcae03l
032600050404      **
032700050404     c                   dow       not %eof(azcae03l)
032800050404      **
032900050405      **   se attivo nel range di date:
033000050404     c                   if        alladat >= caedde and alladat <= caedsc
033100050404      **
033200050404      **  test su AZORG se network DPD
033300050405     c     CAETFA        chain     azorg01l
033400050404     c                   if        %Found(azorg01l)
033500050404     c                   movel     orgde3        og143
033600060503     c*****              if        §OGHUB <> *blank and §OGNTW = 'DPD'
033700130220     c****************   if        §OGHb1 <> *blank and §OGNTW = 'DPD'
033800130220     c*****    il test §OGHB1 non serve avendo già il NETWORK
033900130220     c                   if        §OGNTW = 'DPD'
034000050405     c                   move      'S'           Ntw_DPD_SN
034100050405     c                   leave
034200050405     c                   end
034300050405     c                   end
034400050405      **
034500050405     c                   endif
034600050405      **
034700050404     c     kcae          reade     azcae03l
034800050404     c                   enddo
034900050404      *
035000050405     c                   close     azcae03l
035100050404      *
035200050404     c                   endSR
035300050404     C*--------------------------------------------------------------------
035400030206      *  Elabora la stampa di una tariffa x cooperativa
035500030207     C*--------------------------------------------------------------------
035600030206     c     elabora       begsr
035700030207      *
035701130619     c                   clear                   cettl             1
035800030206     c                   clear                   sav_01
035900030206     c                   clear                   sav_02
036000030206      *
036100030206      *  Inizia il ciclo sulle tariffe riordinate per la stampa
036200030207     c     sav99         do        99            x8a
036300030207     c                   move      ord(x8a)      DS10fld
036400030206      *
036500030206      *  solo quelle abilitate alla stampa
036600030207     c                   if        ord_01 <>'00' and
036700030224     c                             ord_01 <>'  '
036800030206      *
036900030206      * Emissione testata Movimentazione
037000030206     c                   if        ord_01 <> sav_01
037100030206     c                   exsr      testa_mov
037200030206     c                   move      ord_01        sav_01
037300030206     c                   end
037400030206      *
037500030206      * Emissione testata Corrispettivi
037600030206     c                   if        ord_02 <> sav_02
037700030206     c                   exsr      testa_cor
037800030206     c                   move      ord_02        sav_02
037900030206     c                   end
038000030206      *
038100030207      * Se il Livello è quello corretto, va in stampa
038200030207     c                   if        Lvl_05 = Livello_Fil or
038300050405     c                             LvL_05 = 'E' or
038400050405     c                             LvL_05 = 'D' and Ntw_DPD_SN = 'S'
038500030520      *
038600030520      *  Se non è una filiale Poste non esegue la stampa dettaglio
038700030520      *   e non ha stampato neppure la sua testata
038800030520     c                   if        ord_02 <>'08' or
038900030520     c                             ord_02 = '08' and
039000030520     c                             §ogntw = 'PPT'
039100030207      *
039200030207      * Prende le tariffe delle singole voci
039300030207     c                   movel     cod_04        ctar              3
039400030207     c     kfpt          chain     fifpt01l
039500030207     c                   z-add     0             detval
039600030207     c                   move      *blank        Ftpg              1
039700030207     c                   if        %Found(fifpt01l) and fptatb = *blank
039800030207     c                   z-add     fptata        detval
039900030207     c                   move      fptTpg        Ftpg
040000030207     c                   endif
040100030207      *
040200030207     c                   z-add     1             xcd               5 0
040300030207     c     cod_04        lookup    C8a(xcd)                               21
040400030207     c                   clear                   detdes
040500030207     c                   movel     des(xcd)      detdes
040501130619      * se tariffa correlata non presente con flag esclusione la salto
040502130619     c                   if        not %Found(fifpt01l) and esc(xcd) = 'N'
040503130619     c                   iter
040504130619     c                   endif
040505130619     c                   if        %Found(fifpt01l) and esc(xcd) = 'N'
040506130619     c                   move      'X'           cettl
040507130619     c                   endif
040600030207      *
040700030207     c                   if        FTPG = *blank
040800030207     c                   eval      FTPG = Tpg(xcd)
040900030207     c                   end
041000030207      *
041100030207     c                   clear                   detuma
041200030207     c                   select
041300030207     c                   when      FTPG = '0'
041400030207     c                   movel     '100 Kg'      detuma
041500030207      *
041600030207     c                   when      FTPG = '1'
041700030207     c                   movel     'Collo'       detuma
041800030207      *
041900030207     c                   when      FTPG = '2'
042000030207     c                   movel     'Spediz'      detuma
042100030207      *
042200030207     c                   when      FTPG = '3'
042300030207     c                   movel     'Kg.'         detuma
042400030902      *
042500030902     c                   when      FTPG = 'H'
042600030909     c                   movel     'Bancale'     detuma
042700030207      *
042800030207     c                   endSl
042900030207      *
043000030207      *               *-------------------*
043100030207     c                   if        *in01 = *on or cntrig >65
043200030207     c                   write     salpag
043300030207     c                   z-add     10            cntrig
043400030207     c                   Setoff                                       01
043500030207     c                   end
043600030207     c                   write     rigdet
043700030207     c                   add       1             cntrig
043800030207      *               *-------------------*
043900030207     c                   end
044000030207      *
044100030520     c                   end
044200030520      *
044300030206     c                   end
044400030206      *
044500030206     c                   endDO
044600030207      *
044700020308     c                   endsr
044800030207     C*--------------------------------------------------------------------
044900030207      *  Decodifica Testata della Tariffa
045000030207     C*--------------------------------------------------------------------
045100030207     c     Decod_Tes     begsr
045200030206      *
045300030207      * Dati di Testata Cooperativa - Società di Fatturazione
045400030207      *  e sito del Magazzino
045500030207     c                   clear                   copd50
045600030207     c                   clear                   RagSoc
045700030207     c                   clear                   Commit
045800030207     c                   clear                   Appalt
045900030207     c                   clear                   socd50
046000030207     c                   clear                   indd01
046100030207     c                   move      *all'0'       kksc
046200030207     c                   move      apdksc        kksc
046300030207     c     krco          chain     anrco98j
046400030207     c                   if        %Found(anrco98j)
046500030207     c                   movel     sogDES        copd50
046600030207     c                   movel     sogDES        ragSoc
046700030207     c                   movel     sogDES        Appalt
046800030218     c                   movel     apdpdr        alfpdr            7
046900030218     c                   eval      copd50 = %TrimR(copd50) + '(' +
047000030218     c                             alfpdr + ')'
047100030207      * Indirizzo
047200030207     c                   movel     indINDRIZ     VIA
047300030207     c                   movel     indLOCALIT    CITTA
047400030207     c                   movel     indCAP        CAP
047500030207     c                   movel     sogPARTIVA    PARIVA
047600030207     c                   end
047700091218      **
047800091218      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
047900091218      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
048000091218      **
048100091218      **  Aggiunge il Codice Fiscale
048200091218     c                   if        sogcdfisc <> ' ' and indprov <> ' '
048300091218     c                   eval      pariva = %Trim(pariva) +
048400091218     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
048500091218     C                             ': ' + %trimr(sogcdfisc)
048600091218     c                   end
048700091218      **
048800091218     c                   if        indtelex <> ' ' and
048900091218     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
049000091218     c                   eval      pariva = %Trim(pariva) + ' ' + indtelex
049100091218     c
049200091218     c                   end
049300091218      **
049400030213      *
049500030207      * Società Bartolini con cui concordare la tariffa.
049600030213      *  presa dall'anagrafica (APDCSF)
049700101105     c**** apdCSF        chain     ansif01l
049800101105     c****               if        %Found(ansif01l)
049900101105     c****               movel     sifRGS        socd50
050000101105     c****               movel     sifRGS        Commit
050100101105     c****               end
050200110510      ****
050300110510      ****  spostato dopo x prendere la data deccorrenza della tariffa
050400110510     c**********         exsr      decod_societa
050500101105      ****
050600030207      * Indirizzo del P.O.
050700030207     c                   movel     apdpdr        fld3              3
050800030207     c                   move      fld3          fil3              3 0
050900030207     c     fil3          chain     azorg01l
051000030207     c                   if        %Found(azorg01l)
051100030207     c                   movel     orgIND        indd01
051200030520     c                   movel     orgde3        og143
051300050201     c                   movel     orgdf0        og150
051400030220     c                   eval      indD01 = %TrimR(indD01) + ' - ' +
051500030220     c                             %TrimR(OrgLOC) +' (' + OrgPRO + ')'
051600030207     c                   end
051700050201     C*  se la filiale è abilitata al Picking
051800050201     c                   clear                   Picking_Fil       1
051900050201     c                   move      §ogPCK        Picking_Fil
052000050201     C*-
052100030207     c*  per capire se 1/2°livello
052200030207      * cerco il terminal di arrivo
052300030207     c                   clear                   fnlv55ds
052400030207     c                   z-add     fil3          d55lin
052500030207     c                   z-add     alladat       d55drf
052600030207     c                   call      'FNLV55R'
052700030207     c                   parm                    fnlv55ds
052800030207      * Livello
052900030207     c                   move      '1'           Livello_Fil       1
053000030207     c                   if        d55tfp <> d55lin
053100070212      * è un secondo livello solo se non è né terminal di partenza né terminal di arrivo
053200070212     c                             and d55tfa <> d55lin
053300030207     c                   move      '2'           Livello_Fil
053400030207     c                   end
053500030207      *
053600030207      * Aggancia la Tariffa
053700030207      *      per Tipo Servizio
053800030207     c                   move      TipServ       ktsr
053900030207     c                   exsr      fifgt
054000030207      *
054100030207     c                   endsr
054200030207     C*--------------------------------------------------------------------
054300030207      * Aggancia la Tariffa della Cooperativa alla data
054400030207     C*--------------------------------------------------------------------
054500030207     c     fifgt         begsr
054600030207      *
054700030227     c                   clear                   tar_ok            1
054800030227      *
054900030207     c                   setoff                                           99
055000030207     c     kfgt          setll     fifgt03l
055100030207     c                   do        *hival
055200030207     c     kfgt          reade     fifgt03l                               99
055300030207     c   99              leave
055400030207      *
055500030207      * non considero le tariffe che risultano fuori periodo
055600030207     c                   if        fgtddt > alladat or  fgtdst < alladat
055700030207     c                   iter
055800030207     c                   end
055900030227      *
056000030227      * Se richiesta la ristampa di tariffe non ancora stampate
056100030227      *  non deve stamparle
056200030227     c                   if        fgtdts = 0 and lancio ='R'
056300030227     c                   iter
056400030227     c                   end
056500030227      *
056600030227      * Se c'è una tariffa valida
056700030227     c                   move      'S'           tar_ok
056800030227      *
056900110510      ****  Spostata qui la decodifica per impostare la data di decorrenza tariffa
057000110510      *      con la quale decodificare la società.
057100110510     c                   exsr      decod_societa
057200110510      ****
057300030207      * Stampa testata Inizio Documento
057400030224     c                   select
057500030224      *
057600030224     c                   when      TipServ = 'X'
057700030207      *               *-------------------*
057800030207     c                   write     testa
057900030207      *               *-------------------*
058000030207     c                   z-add     15            cntrig
058100030224      *
058200030224     c                   when      TipServ = 'P'
058300030224      *               *-------------------*
058400030224     c                   write     tesPul
058500030224      *               *-------------------*
058600030227     c                   z-add     14            cntrig
058700030224      *
058800030224     c                   when      TipServ = 'T'
058900030224      *               *-------------------*
059000030224     c                   write     tesTra
059100030224      *               *-------------------*
059200030227     c                   z-add     16            cntrig
059300030224     c                   endsl
059400030207      *
059500030207      * se richiesta stampa in modalità di convalida aggiorno le due date
059600030207      * convalida e stampa
059700090116     c                   if        lancio = 'C'
059800090116     c                               and fgtdts = 0
059900030207     c                   move      dataoggi      fgtdts
060000030207     c                   update    fifgt000
060100030207     c                   endif
060200030207      *
060300030207      * se impostato riferimento altra tariffa inserisco il loop per agganciare
060400030207      * la tariffa con il codice di riferimento valida e proseguo normalmente
060500030207     c                   setoff                                       76
060600030207     c                   if        fgtrct <> 0 and fgtrap <> 0
060700030207      *
060800030207     C                   z-add     fgtrct        krct
060900030207     c                   z-add     fgtrap        krap
061000030207     c                   clear                   trovata           1
061100030207     c     kfgtx         setll     fifgt01l
061200030207      *
061300030207     c                   do        *hival
061400030207     c     kfgtx         reade     fifgt01l                               96
061500030207     c   96              leave
061600030207      *
061700030207      * non considero le tariffe che risultano fuori periodo
061800030207     c                   if        fgtddt > alladat or fgtdst < alladat or
061900030207     c                             fgtatb <> *blank
062000030207     c                   iter
062100030207     c                   end
062200030207     c                   move      'X'           trovata
062300030207     c                   leave
062400030207     c                   enddo
062500030207      *
062600030207      * non trovo nessuna tariffa valida con i riferimenti richiesti lo segnalo
062700030207     c                   if        trovata = *blank
062800030207     c                   iter
062900030207     c                   else
063000030207     c                   seton                                        76
063100030207     c                   endif
063200030207      *
063300030207     c                   endif
063400030207      *
063500030224      * giro le date per la stampa
063600030207     c                   if        fgtddt > 0
063700030207     c                   move      fgtddt        dataiso
063800030207     c                   move      dataiso       dataeur
063900030218     c                   move      dataeur       data10
064000040817     c                   leave
064100030207     c                   end
064200030207      *
064300030207     c                   enddo
064400030207      *
064500030207     c                   endsr
064600030207     C*--------------------------------------------------------------------
064700030224      *  elabora tarizza x pulizie/trattoristi
064800030207     C*--------------------------------------------------------------------
064900030224     c     ElabTar       begsr
065000030224      *
065100030224      *  Inizia il ciclo sulle tariffe riordinate per la stampa
065200030224     c     sav99         do        99            x8a
065300030224     c                   move      ord(x8a)      DS10fld
065400030227      *
065500030227     c                   if        TipServ = 'P'
065600030227     c                   eval      rigimp = pulizC
065700030227     c                   endIf
065800030227      *
065900030227     c                   if        TipServ = 'T'
066000030227     c                   eval      rigimp = TrattC
066100030227     c                   endIf
066200030224      *
066300030224      * Prende le tariffe della singola voci
066400030224     c                   movel     cod_04        ctar              3
066500030224     c     kfpt          chain     fifpt01l
066600030227     c                   z-add     0             tarser           10 3
066700030227      *
066800030224     c                   if        %Found(fifpt01l) and fptatb = *blank
066900030227      *
067000030227      * essendo espresso mensilmente per rapportarlo all'anno moltiplica x 12
067100030227     c                   if        TipServ = 'P'
067200030227     c     fptata        mult      12            tarser
067300030227     c                   endIf
067400030227      *
067500030227      * per i trattoristi invece è corretto mensile
067600030227     c                   if        TipServ = 'T'
067700030227     c                   z-add     fptata        tarser
067800030227     c                   endIf
067900030227      *
068000030227      * adatta il numero in un campo alfa da concattare alla stringa
068100030227      * che compone la riga da stampare
068200030227     c                   move      tarser        atanum           10
068300030227     c                   movea     atanum        numa
068400030227     c                   clear                   numa1
068500030227      * per riportare su campo alfa il valore della tariffa
068600030227     c                   exsr      Adatta_num
068700030227     c                   movea     numa1         alfnum           11
068800030227      *
068900030227     c                   eval      RigImp = %TrimR(RigImp) + '  *' +
069000030227     c                             %TrimL(alfnum) + '* '
069100030224     c                   endif
069200030224      *
069300030224      *  Decodifica in Lettere il valore della tariffa
069400030226     c                   z-add     tarSer        SALNUM
069500030226     c                   move      tarSer        DECNUM
069600030226     c                   exsr      NUMLET
069700030227      *
069800030227      * Solo per il Contratto di pulizia
069900030227      *  adatta le parole della riga in base alla cifra della tariffa
070000030227      *  quindi deve calcolare dove interrompere la costante per portarla
070100030227      *  a capo della riga.
070200030227     c                   IF        TipServ = 'P'
070300030227     c                   eval      Implet = '(' + %TrimR(savlet) +
070400030227     c                             ') oltre IVA, '
070500030227      *
070600030227      * adatta la stringa della costante in base alla cifra della tariffa
070700030227     c                   Exsr      Adatta_Rate
070800030227      *
070900030227      * 'che verrà suddiviso in n.12 rate mensili di pari importo di Euro'
071000030227     c                   else
071100030227     c                   eval      Implet = '(' + %TrimR(savlet) +
071200030227     c                             ') oltre IVA.'
071300030227     c                   end
071400030224      *
071500030224      * Non è previsto il salto pagina x OVRFLOW
071600030224      * stampa l'importo tariffa con delega x fattura finale
071700030224      *               *-------------------*
071800030224     c                   write     imptar
071900030227     c                   add       2             cntrig
072000030224      *               *-------------------*
072100030224      *  solo per Pulizie
072200030224     C                   IF        TipServ = 'P'
072300030227      *  definisce le rate mensili adattando alla riga le parole
072400030227      *  da utilizzare nel contratto.
072500030227     c                   z-add     fptata        tarmes           10 3
072600030224      *  Decodifica in Lettere il valore della tariffa
072700030227     c                   z-add     tarMes        SALNUM
072800030227     c                   move      tarMes        DECNUM
072900030227     c                   exsr      NUMLET
073000030227      *
073100030227      * adatta il numero in un campo alfa da concattare alla stringa
073200030227      * che compone la riga da stampare
073300030227     c                   move      tarmes        atanum           10
073400030227     c                   movea     atanum        numa
073500030227     c                   clear                   numa1
073600030227      * per riportare su campo alfa il valore della tariffa
073700030227     c                   exsr      Adatta_num
073800030227     c                   movea     numa1         alfnum           11
073900030227      *
074000030227      * adatta la 2°Riga in base alla cifra del mese ottenuta
074100030227     c                   Exsr      Adatta_2Rig
074200030227      *               *-------------------*
074300030224     c                   write     impmes
074400090115     c                   clear                   rigmes
074500030227     c                   add       1             cntrig
074600030227      *               *-------------------*
074700030227     c                   if        rigcnt <> *blank
074800030227      *               *-------------------*
074900030227     c                   write     Cntmes
075000030227     c                   add       1             cntrig
075100030227      *               *-------------------*
075200030227     c                   end
075300030224     c                   endIF
075400030224      *               *-------------------*
075500030224     c                   write     delfat
075600030224     c                   add       3             cntrig
075700030224      *               *-------------------*
075800030224      *
075900030224     c                   endDO
076000030224      *
076100030224     c                   endsr
076200030224     C*--------------------------------------------------------------------
076300030224      *  emissione Testata Movimentazione
076400030224     C*--------------------------------------------------------------------
076500030224     c     Testa_Mov     begsr
076600030224      *
076700030206     c                   clear                   ts1ORD
076800030207     c                   setoff                                       44
076900030206      *
077000030206     c                   Select
077100030206      *  Collettame
077200030206     c                   when      ord_01 = '01'
077300030206     c                   movel     movim1        ts1ORD
077400030206      *  Pacchi Poste
077500030206     c                   when      ord_01 = '02'
077600030206     c                   movel     movim2        ts1ORD
077700030207     c                   seton                                        44
077800030206     c                   endsl
077900030206      *
078000030206      *               *--------------------*
078100030207     c   44              if        *in01 = *on or cntrig >62
078200030207     c                   write     salpag
078300030207     c                   z-add     10            cntrig
078400030207     c                   Setoff                                       01
078500030207     c                   end
078600030207     c   44              write     COST01
078700030207     c   44              add       2             cntrig
078800030207      *               *--------------------*
078900030207     c                   if        *in01 = *on or cntrig >62
079000030207     c                   write     salpag
079100030207     c                   z-add     10            cntrig
079200030207     c                   Setoff                                       01
079300030207     c                   end
079400030206     c                   write     Testa1
079500030207     c                   add       1             cntrig
079600030207      *               *--------------------*
079700030207     c   44              if        *in01 = *on or cntrig >62
079800030207     c                   write     salpag
079900030207     c                   z-add     10            cntrig
080000030207     c                   Setoff                                       01
080100030207     c                   end
080200030207     c   44              write     COST02
080300030207     c   44              add       2             cntrig
080400030206      *               *--------------------*
080500030206      *
080600030206     c                   endsr
080700030207     C*--------------------------------------------------------------------
080800030206      *  emissione Testata Corrispettivi
080900030207     C*--------------------------------------------------------------------
081000030206     c     Testa_Cor     begsr
081100030206      *
081200030206     c                   clear                   ts1ORD
081300030206      *
081400030206     c                   Select
081500030206      *  Corr.ciclo Partenze
081600030206     c                   when      ord_02 = '01'
081700030206     c                   movel     corr01        ts1ORD
081800030206      *  Corr.ciclo Arrivi
081900030206     c                   when      ord_02 = '02'
082000030206     c                   movel     corr02        ts1ORD
082100030206      *  Corr.merci in transito-Arrivi
082200030207     c                   when      ord_02 = '03' and Livello_Fil = '1'
082300030206     c                   movel     corr03        ts1ORD
082400030206      *  Corr.merci in transito-Partenze
082500030207     c                   when      ord_02 = '04' and Livello_Fil = '1'
082600030206     c                   movel     corr04        ts1ORD
082700030206      *  Corr.Giacenze Magazzino
082800030206     c                   when      ord_02 = '05'
082900030206     c                   movel     corr05        ts1ORD
083000030206      *  Corr.ciclo Internazionale
083100030207     c                   when      ord_02 = '06' and Livello_Fil = '1'
083200030206     c                   movel     corr06        ts1ORD
083300030206      *  Corr.ciclo Arrivi Pacchi Poste
083400030206     c                   when      ord_02 = '07'
083500030206     c                   movel     corr07        ts1ORD
083600030512      *  Corr.ciclo Partenze Pacchi Poste
083700030512     c                   when      ord_02 = '08'
083800030512     c                   movel     corr08        ts1ORD
083900030206     c                   endsl
084000030520      *
084100030520      *  Se non è una filiale Poste
084200030520     c                   if        ord_02 = '08' and
084300030520     c                             §ogntw <>'PPT'
084400030520     c                   goto      endtescor
084500030520     c                   end
084600030206      *
084700030206      *               *--------------------*
084800030207     c                   if        *in01 = *on or cntrig >62
084900030207     c                   write     salpag
085000030207     c                   z-add     10            cntrig
085100030207     c                   Setoff                                       01
085200030207     c                   end
085300030206     c                   write     Testa1
085400030207     c                   add       1             cntrig
085500030206      *               *--------------------*
085600030206      *
085700030520     c     endtescor     endsr
085800030207     C*--------------------------------------------------------------------
085900030207      * Dopo il dettaglio a conclusione stampa il finale della tariffa
086000030207     C*--------------------------------------------------------------------
086100030207     c     Fine_tar      begsr
086200030207      *
086300050125      *  Imposta Fincature x C/C Bancario dati ABI CAB
086400080208     C                   movel     CBan_sep      CCBANC           74
086500080208     C                   movel     CABI_sep      CodABI           35
086600080208     C                   movel     CCAB_sep      CodCAB           38
086700080208     C                   movel     CCIBAN_sep    CCIBAN
086800050125      *
086900030224      *  Per tariffa Cooperative - Voci
087000030224     C                   IF        TipServ = 'X'
087100030224      *
087200030207      *               *--------------------*
087300030207     c                   if        *in01 = *on or cntrig >60
087400030207     c                   write     salpag
087500030207     c                   z-add     10            cntrig
087600030207     c                   Setoff                                       01
087700030207     c                   end
087701130619     c                   if        cettl = 'X'
087800030207     c                   write     FINE01
087801130619     c                   endif
087802130619     c                   write     FINE01b
087900030207     c                   add       2             cntrig
088000030207      *               *--------------------*
088100030207     c                   if        *in01 = *on or cntrig >60
088200030207     c                   write     salpag
088300030207     c                   z-add     10            cntrig
088400030207     c                   Setoff                                       01
088500030207     c                   end
088600030207     c                   write     FINE02
088700030207     c                   add       3             cntrig
088800030207      *               *--------------------*
088900030207     c                   if        *in01 = *on or cntrig >60
089000030207     c                   write     salpag
089100030207     c                   z-add     10            cntrig
089200030207     c                   Setoff                                       01
089300030207     c                   end
089400030207     c                   write     FINE03
089500030207     c                   add       3             cntrig
089600030207      *               *--------------------*
089700030207     c                   if        *in01 = *on or cntrig >60
089800030207     c                   write     salpag
089900030207     c                   z-add     10            cntrig
090000030207     c                   Setoff                                       01
090100030207     c                   end
090200030207     c                   write     FINE04
090300030207     c                   add       2             cntrig
090400030207      *               *--------------------*
090500030207     c                   if        *in01 = *on or cntrig >55
090600030207     c                   write     salpag
090700030207     c                   z-add     10            cntrig
090800030207     c                   Setoff                                       01
090900030207     c                   end
091000030207     c                   write     FINE05
091100030207     c                   add       7             cntrig
091200030207      *               *--------------------*
091300030207     c                   if        *in01 = *on or cntrig >52
091400030207     c                   write     salpag
091500030207     c                   z-add     10            cntrig
091600030207     c                   Setoff                                       01
091700030207     c                   end
091800030207     c                   write     FINE06
091900030224     c                   add       7             cntrig
092000030207      *               *--------------------*
092100030207      *
092200030224     C                   ELSE
092300030224      *
092400030224      *  Per tariffa Cooperative - Pulizie/Trattoristi
092500030224      *   quindi chiude il contratto impostando i dati anagrafici
092600030224      *    e della banca quindi il finale del contratto.
092700030224      *               *--------------------*
092800030224     c********           if        *in01 = *on or cntrig >55
092900030224     c********           write     salpag
093000030224     c********           z-add     10            cntrig
093100030224     c********           Setoff                                       01
093200030224     c********           end
093300030224     c                   write     FINE05
093400030224     c                   add       7             cntrig
093500030224      *               *--------------------*
093600030224     c********           if        *in01 = *on or cntrig >52
093700030224     c********           write     salpag
093800030224     c********           z-add     10            cntrig
093900030224     c********           Setoff                                       01
094000030224     c********           end
094100030224     c                   write     FINE06
094200030224     c                   add       7             cntrig
094300030224      *               *--------------------*
094400030224      *  Pulizie
094500030224     C                   if        TipServ = 'P'
094600030224     c                   write     FINPUL
094700030224     c                   add       3             cntrig
094800030224     c                   end
094900030224      *
095000030224      *  Trattoristi
095100030224     C                   if        TipServ = 'T'
095200030224     c                   write     FINTRA
095300030224     c                   add       4             cntrig
095400030224     c                   end
095500030224      *
095600030224     C                   END
095700030224      * chiusura firme
095800030224     c                   write     FINE07
095900030224     c                   add       3             cntrig
096000030224      *
096100030207     c                   endsr
096200030207     C*--------------------------------------------------------------------
096300030207     c     *inzsr        begsr
096400030207     C*--------------------------------------------------------------------
096500020304     c     *entry        plist
096600020304     c                   parm                    kpjba
096700020304     c                   movel     kpjbu         param
096800020304
096900030206     C     ktab_02       klist
097000030206     C                   kfld                    codut             1 0
097100030206     C                   kfld                    cod
097200030207      *
097300030206     C     ktab          klist
097400030206     C                   kfld                    codut
097500030206     C                   kfld                    cod
097600030206     C                   kfld                    key
097700030207      *
097800030206     c     kapd          klist
097900030206     c                   kfld                    atip
098000030206     c                   kfld                    kpdr
098100030207      *
098200030206     c     krco          klist
098300030206     c                   kfld                    apdcsf
098400030206     c                   kfld                    knatura           1
098500030206     c                   kfld                    kksc              8
098600030207      *
098700020304     c     kfgt          klist
098800030207     c                   kfld                    apdpdr
098900020304     c                   kfld                    ksml
099000020304     c                   kfld                    ktsr
099100030207      *
099200020313     c     kfgtx         klist
099300020313     c                   kfld                    krap
099400020313     c                   kfld                    fgtsml
099500020313     c                   kfld                    fgttsr
099600020313     c                   kfld                    krct
099700030207      *
099800030207      *------
099900020304     c     kfpt          klist
100000020304     c                   kfld                    fgtpdr
100100020304     c                   kfld                    fgtsml
100200020304     c                   kfld                    fgttsr
100300020304     c                   kfld                    fgtctr
100400040817     c                   kfld                    fgtprg
100500030207     c                   kfld                    CTar
100600030206      *------
100700030207     c                   movel     'F'           knatura
100800030207     c                   clear                   ksml
100900030207     c                   movel     'C'           atip              1
101000030206     c                   z-add     1             codut
101100030207     c                   z-add     codpadr       kpdr
101200030206     C*
101300030206      *  In base all'utente prende il P.O.
101400030206     C                   CLEAR                   Tibs36Ds
101500030206     c                   EVAL      I36ute = Knmus
101600030206     c                   EVAL      I36Tla = 'L'
101700030206     C                   CALL      'TIBS36R'
101800030206     C                   PARM                    Tibs36Ds
101900030206      * se utente di sede
102000030206     c                   eval      utesed ='N'
102100030206     c                   if        o36pou = 046
102200030206     c                   eval      utesed ='S'
102300030206     c                   end
102400030206     C*-
102500030206     C                   Z-ADD     1             CODUT
102600030206     C                   CALL      'X§PARUT'
102700030206     C                   PARM                    UT§DSE
102800030206     C                   MOVEL     REC80         CNCR80
102900030213     C                   MOVEL     REC41         CNCR41
103000030213     C                   MOVEL     REC42         CNCR42
103100030206     C*-
103200020313     c     *like         define    fgtrap        krap
103300020313     c     *like         define    fgtrct        krct
103400020313     C     *LIKE         DEFINE    TBLCOD        COD
103500020305     C     *LIKE         DEFINE    TBLKEY        KEY
103600020305     C     *LIKE         DEFINE    fgtpdr        kpdr
103700020305     C     *LIKE         DEFINE    fgtsml        Ksml
103800020305     C     *LIKE         DEFINE    fgttsr        Ktsr
103900040817     c     *like         define    fgtprg        Kprg
104000020305
104100020218     C                   time                    w0120            14 0
104200020218     C                   move      w0120         wdat              8 0
104300020308      * data da impostare su data stampa e convalida se lancio = 'C'
104400020308     c                   move      wdat          dataeur
104500020308     c                   move      dataeur       dataiso
104600020308     c                   move      dataiso       dataoggi          8 0
104700020308      * data da elaborare
104800020304     c                   move      alladat       dataiso
104900020304     c                   move      dataiso       dataeur
105000020304     c                   move      dataeur       alladata          8 0
105100030227      *
105200030227     c                   clear                   modo
105300020402     c                   select
105400020402     c                   when      lancio = 'C'
105500020411     c                   seton                                        70
105600030227     c                   when      lancio = 'R' and
105700030227     c                             autconv <> 'S'
105800020402     c                   eval      modo = 'Ristampa'
105900020402     c                   when      lancio = 'P'
106000020402     c                   eval      modo = 'Prova   '
106100020402     c                   endsl
106200030203     c
106300011026     c                   endsr
106400030207     C*--------------------------------------------------------------------
106500030227     C* Adatta la costante delle rate mensili per le pulizie nelle 2 righe
106600030227     C*--------------------------------------------------------------------
106700030227     C     Adatta_Rate   BEGSR
106800030227     c*
106900030227     c* Calcola lo spazio residuo sulla prima riga
107000030227     c                   clear                   xlung             3 0
107100030227     c                   eval      xlung = %len(%TrimR(Implet))
107200030227     c                   eval      nm = 105 - xlung
107300030227      * Imposta la costante in schiera
107400030227     c                   movea     pulrat        pul65
107500030227      *
107600030227      * se lo spazio disponibile è inferiore a tutta la costante
107700030227     c                   if        nm < 65
107800030227     c                   dow       pul65(nm) <> ' '
107900030227     c                   sub       1             nm
108000030227     c                   enddo
108100030227      * prende il primo spazio precedente alla parola disponibile
108200030227     c                   sub       1             nm
108300030227     c                   clear                   fld65            65
108400030227      * quindi aggiunge alla riga le parole disponibili senza spezzarle
108500030227     c                   eval      fld65 = %subst(pulrat: 1: nm)
108600030227     c                   eval      Implet = %TrimR(Implet) +
108700030227     c                             fld65
108800030227      * il resto della frase viene riportato sulla riga successiva
108900030227     c                   eval      RigMes = %subst(pulrat: nm+2: 64-nm)
109000030227      *
109100030227     c                   else
109200030227      *
109300030227      * se lo spazio disponibile è sufficiente non deve fare altro che
109400030227      *  aggiungere la costante.
109500030227     c                   eval      Implet = %TrimR(Implet) +
109600030227     c                             pulrat
109700030227     c                   end
109800030227      *
109900030227     c                   endsr
110000030227     C*--------------------------------------------------------------------
110100030227     C* Adatta la seconda riga delle rate mensili
110200030227     C*--------------------------------------------------------------------
110300030227     C     Adatta_2rig   BEGSR
110400030227     c*
110500030227     c* Calcola lo spazio residuo sulla prima riga
110600030227     c                   clear                   xlung             3 0
110700030227     c                   eval      xlung = %len(%TrimR(RigMes))
110800030227     c                   eval      nm = 105 - xlung
110900030227      *
111000030227      * se lo spazio disponibile è inferiore a tutta la costante
111100030227     c                   if        nm > 10
111200030227      * il resto della frase viene riportato sulla riga successiva
111300030227     c                   eval      RigMes = %TrimR(Rigmes) + ' *' +
111400030227     c                             %Trim(alfnum) + '*'
111500030227     c                   eval      RigMes = %TrimR(Rigmes) + ' (' +
111600030227     c                             %TrimR(savlet) + ')'
111700030227      *
111800030227      *  ultima dicitura da adattare nella riga
111900030227     c                   exsr      Adatta_IVA
112000030227      *
112100030227     c                   end
112200030227      *
112300030227     c                   endsr
112400030227     C*--------------------------------------------------------------------
112500030227     C* aggiunge a fianco oppure si serve di una riga aggiuntiva
112600030227     C*--------------------------------------------------------------------
112700030227     C     Adatta_IVA    BEGSR
112800030227      *
112900030227     c* Calcola lo spazio residuo sulla prima riga
113000030227      *  per aggiungere (ciascuna, oltre IVA.)
113100030227     c                   clear                   xlung             3 0
113200030227     c                   eval      xlung = %len(%TrimR(RigMes))
113300030227     c                   eval      nm = 105 - xlung
113400030227      *
113500030227      * Imposta la costante in schiera
113600030227     c                   movea     oltiva        pul21
113700030227      *
113800030227      * se lo spazio disponibile è inferiore a tutta la costante
113900080618     c                   if        nm < 21 and nm > 0
114000030227     c                   dow       pul21(nm) <> ' '
114100030227     c                   sub       1             nm
114200080618     c                   if        nm = 0
114300080618     c                   leave
114400080618     c                   end
114500030227     c                   enddo
114600030227      * prende il primo spazio precedente alla parola disponibile
114700030227     c                   sub       1             nm
114800030227     c                   clear                   fld21            21
114900030227      * quindi aggiunge alla riga le parole disponibili senza spezzarle
115000030227     c                   eval      fld21 = %subst(oltiva: 1: nm)
115100030227     c                   eval      RigMes = %TrimR(RigMes) +
115200030227     c                             fld21
115300030227      * il resto della frase viene riportato sulla riga successiva
115400030227     c                   eval      RigCnt = %subst(oltIva: nm+2: 20-nm)
115500030227      *
115600030227     c                   else
115700030227      *
115800030227      * se lo spazio disponibile è sufficiente non deve fare altro che
115900030227      *  aggiungere la costante.
116000030227     c                   eval      RigMes = %TrimR(RigMes) +
116100030227     c                             oltIVA
116200030227     c                   end
116300030227      *
116400030227     c                   endsr
116500030227     C*--------------------------------------------------------------------
116600030227     C* trasforma il numero in un campo alfa da utilizzare con %Trim
116700030227     C*--------------------------------------------------------------------
116800030227     C     Adatta_Num    BEGSR
116900030227     c*
117000030227      * reimposta il numero su schiere alfanumeriche per ricomporlo nel campo
117100030227      * variabile
117200030227     c                   clear                   numok             1
117300030227     c                   do        10            nm                3 0
117400030227     c                   if        numa(nm) ='0' and numok <> '1'
117500030227     c                   move      *blank        numa1(nm)
117600030227     c                   else
117700030227     c                   move      '1'           numok
117800030227     c                   endif
117900030227     c                   if        numok = '1'
118000030227      * imposta la virgola
118100030227     c                   z-add     nm            xnm               3 0
118200030227     c                   if        nm = 8
118300030227     c                   move      ','           numa1(xnm)
118400030227     c                   end
118500030227     c                   if        nm >= 8
118600030227     c     nm            add       1             xnm
118700030227     c                   end
118800030227     c                   move      numa(nm)      numa1(xnm)
118900030227     c                   endif
119000030227     c                   enddo
119100030227     c*
119200030227     c                   endsr
119300030227     C*--------------------------------------------------------------------
119400030227     C* TRASFORMO L'IMPORTO IN LETTERE
119500030227     C*--------------------------------------------------------------------
119600030227     C     NUMLET        BEGSR
119700030227     C*
119800030226     C                   MOVE      *BLANKS       LETTER
119900030226     C                   MOVE      0             BYTE
120000030226     C                   MOVEL     SALNUM        INTERO
120100030226     C                   Z-ADD     INTERO        NUMER
120200030226     C                   MOVE      *zeros        DECIM
120300030226      *
120400030227     C                   MOVEL     kpjbu         savkpj
120500030227     C                   MOVEL     PARAM3        KPJBU
120600030226     C                   CALL      'XNITAL'
120700030226     C                   PARM                    KPJBA
120800030226     C                   MOVEL     KPJBU         PARAM3
120900030227     C                   MOVEL     savkpj        kpjbu
121000030226      *
121100030226     C                   MOVE      *BLANK        ALF
121200030226     C                   MOVEA     LETTER        ALF
121300030227     C**** DECNUM        IFNE      '000'
121400030226     C                   Z-ADD     2             XJ
121500030226     C     '*'           LOOKUP    ALF(XJ)                                21
121600030227     C   21              if        XJ < 56
121700030226     C                   MOVE      '/'           ALF(XJ)
121800030226     C                   ADD       1             XJ
121900030226     C                   MOVEA     DECNUM        ALF(XJ)
122000030226     C                   ADD       3             XJ
122100030226     C                   MOVEA     '*'           ALF(XJ)
122200030226     C                   END
122300030227     C******             END
122400030227     C                   MOVEA     ALF           SavLET
122500030226     C*
122600030226     C                   ENDSR
122700030226     C**************************************************************************
122800101105     C* decodifica la società
122900101105     C************************************************************
123000101105     C     Decod_societa begSR
123100101105      *
123200101105     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
123300101105     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
123400101105     C                   RESET                   tibsSocI0
123500101105     C                   EVAL      tibsSocI0.idSocieta = apdCSF
123600110510     C                   move      fgtDDT        dataiso
123700110510     C                   EVAL      tibsSocI0.dtVALIDITA= dataiso
123800101105     C
123900101105     C                   CALL      'TIBSSOCR'
124000101105     C                   PARM      'GETANAGRAF'  prmRqsOpCode
124100101105     C                   PARM      *BLANK        prmRpyOpCode
124200101105     C                   PARM      *ZERO         prmRpyIdMsg
124300101105     C                   PARM      'TIBSSOCI0'   prmRqsFormato
124400101105     C                   PARM                    tibsSocI0
124500101105     C                   PARM                    prmRqsDataSize
124600101105     C                   PARM      'TIBSSOCO0'   prmRpyFormato
124700101105     C                   PARM                    tibsSocO0
124800101105     C                   PARM                    prmRpyDataSize
124900101105     c                   if         PRMRPYIDMSG >= 0
125000101105     c                              and TIBSSOCO0.IDSOCIETA <> *blank
125100101105     c                   eval       socd50 = tibsSocO0.RAGSOCIALE
125200101105     c                   eval       Commit = tibsSocO0.RAGSOCIALE
125300101105     c                   end
125400101105      *
125500101105     C                   ENDSR
125600101105     C************************************************************
