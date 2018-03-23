000100960530      ***********************************************************************
000200120821      *   ESPORTA DATI BOLLE x GEL  tracciato NETEXPRESS  Export            *
000300120821      *     ?   - NUOVO TRACCIATO -           NETEXPRESS      ?
000400121217      ***********************************************************************
000500121217      *
000600121217      *     ?  PARTICOLARITA':  ?
000700121217      *
000800121217      *    CHIODI : --> x ELECTROLUX 1160985 come mittente, si deve inviare
000900121217      *    con il tipo servizio consegna entro le 12:00:SSERVICE='M' anziché 'O'.
001000121217      *
001100960530      ***********************************************************************
001200891004     H DECEDIT('0,') DATEDIT(*DMY/)
001300000207      *---------------------------------------------------------------------*
001400970204     FFNARB01L  UF   E           K DISK    COMMIT
001500120822     Ffiar401L  IF   E           K DISK
001600120822     Ffiar501L  IF   E           K DISK
001700120822     Ffiar901L  IF   E           K DISK
001800020226     FFNART01L  IF   E           K DISK
001900051017     FFIARS01L  IF   E           K DISK
002000121008     Ffnlbl01L  IF   E           K DISK
002100960530     FTABEL00F  IF   E           K DISK
002200120822     FEDIFCSUM  iF A E             DISK
002300000207      *---------------------------------------------------------------------*
002400120822      * Definizioni Varie
002500000207      *---------------------------------------------------------------------*
002600121008     D Bolla_di_Reso...
002700121008     d                 s              1
002800121008     D Bolla_da_ORM...
002900121008     d                 s              1
003000121008     D dsBL4m        E DS
003100121009     D dsBL4e        E DS
003200120822     D TRTC83DS1     E DS
003300120822     D EDGEL00S      e DS
003400120822     D EDGEL00T      e DS
003500120822      *
003600120822     D ARBSPE16        DS            16    INZ
003700120822     D ARBSPE14                3     16
003800120822     D  arbAAS                 1      4  0
003900120822     D  arbLNP                 5      7  0
004000120822     D  arbNRS                 8      9  0
004100120822     D  arbNSP                10     16  0
004200120822      *
004300120822     D dar5gen       e ds
004400120822      *
004500120822     D Num             S              1    DIM(15)
004600120822      * Codici bolla
004700120822     D C3A             S              2    DIM(200)
004800120822     D D3A             S             89    DIM(200)
004900000207      *---------------------------------------------------------------------*
005000000207      * DS
005100000207      *---------------------------------------------------------------------*
005200010424      *  Ds per reperimento tabella PT e campo risultato
005300020219     D Dati            S           1000
005400000207      *---------------------------------------------------------------------*
005500960530     D DS3A          E DS
005600961126     D DS7A2         E DS
005700030521     D DSQT1         E DS                  INZ
005800120822     D*-------------
005900950428     D KPJBA         E DS
006000960530     D WLBDA8          DS                  INZ
006100950428     D  G02DAT                 1      8  0
006200950428     D  G02INV                 9     16  0
006300950428     D  G02ERR                17     17
006400950428     D  G02TGI                18     22  0
006500020222     C***********************************************************************
006600120822     D DepotArrivo     C                   CONST('DE1')
006700120822     D DepotMittente   C                   CONST('IT1')
006800120822     D digits          C                   CONST('0123456789')
006900120822     C***********************************************************************
007000120821     IRRIFCSUM
007100120821     I              RRIFCSUM                    SUMDAT
007200120822     C***********************************************************************
007300020222     C*---------------------------------------------------------------------*
007400960530     C* Ciclo principale                                                   -*
007500960530     C*---------------------------------------------------------------------*
007600010426     C     *ENTRY        PLIST
007700010426     C                   PARM                    KPJBA
007800120822     C                   MOVEL     KPJBU         TRTC83DS1
007900010426     C*
008000960530     C*  Se c'è numero spedizione scrivo dettaglio
008100020227    2C     F83NSP        IFNE      0
008200010426     C*
008300010426     C*-------------------
008400960530     C*  Eseguo posizionamento su archivio bolle per passaggio param.
008500020227     C                   Z-ADD     F83AAS        KAAS
008600020227     C                   Z-ADD     F83LNP        KLNP
008700020227     C                   Z-ADD     F83NRS        KNRS
008800020227     C                   Z-ADD     F83NSP        KNSP
008900020220      * B O L L A
009000960530     C     KARB          CHAIN     FNARB01L                           31
009100020219      *
009200970124    3C     *IN31         IFEQ      *OFF
009300020219      * ------------
009400020220     C* COMPOSIZIONE record BOLLA
009500020219     C                   clear                   Dati
009600120822     C* dati bolla   (S)
009700120822     c                   exsr      dati_ARB
009800120822     C* dati segnacolli (T)
009900120822     c                   exsr      dati_ART
010000970124    3C                   END
010100020219     C*
010200960530     C*  Fleggo FNARB come trasmesso
010300020227     c                   if        F83DFV >0
010400960530     C                   MOVEL     'T'           ARBFT3
010500020227     C                   Z-ADD     F83DFV        ARBDT3
010600020227     c                   end
010700020227     C*
010800020227     C                   update    fnarb000
010900970124     C                   SETON                                        RT
011000970124     C*
011100970124   X2C                   ELSE
011200020219     C*
011300970124     C                   SETON                                        LR
011400020227     C*
011500970124    2C                   END
011600960530     C*--------------------------------------------------------------*
011700020219     C* dettaglio spedizione
011800960530     C*--------------------------------------------------------------*
011900120822     C     Dati_ARB      BEGSR
012000120822     C*
012100120822     C* OCCORRE FARE Attenzione ai campi alfanumerici se contengono dei (;)
012200120822     C*  xchè devono essere sostituiti altrimenti creano problemi con i
012300120822     C* (;) di separazione dei campi.
012400120822     C*
012500120822     C*  UTILIZZANDO XLATE sui campi alfa vengono sostituiti con SPAZI.
012600120822     C*
012700120822     C*----------------
012800120822     C*  Anche se al momento non viene utilizzato predispongo la decodifica.
012900120822     C*  del Porto.
013000120822     C                   z-add     1             Y                 3 0
013100120822     C     arbCBO        LOOKUP    C3A(Y)                                 32
013200120822     C   32              MOVEL     D3A(Y)        DS3A
013300120822     C* Se franco frontiera imposto 'A' come se fosse un porto
013400120822     C* assegnato
013500120822     C                   MOVEL     §3ATB1        WPOR              1
013600120822     C     ARBFBR        IFEQ      'F'
013700120822     C                   MOVEL     'A'           WPOR
013800120822     C                   END
013900120822     C     §3ATB1        IFEQ      'FS'
014000120822     C                   MOVEL     'A'           WPOR
014100120822     C                   END
014200120822     C     §3ATB1        IFEQ      'AS'
014300120822     C                   MOVEL     'F'           WPOR
014400120822     C                   ENDIF
014500120822     C*----------------
014600120822     C*
014700120822     C*  Il volume/peso da CML o da Bollettazione reale
014800120822     c                   exsr      VOL_PES
014900120822     C*
015000120822     C                   Z-ADD(H)  wpes          WPKF             12 1
015100120822     C     WPKF          IFEQ      0
015200120822     C                   Z-ADD     1             WPKF
015300120822     C                   END
015400120822      * x GEL
015500120822     C* devono essere impostati senza virgola e con un solo decimale
015600120822     C* quindi occorre moltiplicarli x 10
015700121005     c                   eval      wpes10 = wpKF * 10
015800121016      * --- in un primo momento si doveva passare lo stesso valore dei kg reali
015900121016      * --- poi con il coefficiente della tariffa (125) si prende il volume
016000121016      * --- e si crea il peso Volumato o tassabile:
016100121016     c                   eval      wvol30 = wvol * 1250
016200120822      * ----------+
016300120822     C*  Reperisco seconda parte ragione sociale destinatario
016400120822     C*    attenzione x Bartolini la rag.soc. è su 2 righe x tot.70 chars.
016500120822     C*     ed il secondo pezzo è su fiar4.......
016600120822     C* Si può effettuare un concatenamento dei campi.
016700120822     C                   MOVEL     'D'           KTRC
016800120822     c                   clear                   RAGdestADD       35
016900120822     C     KAR4          CHAIN     fiar401L
017000120822     c                   if        %Found(fiar401L)
017100120822     c                   eval        RAGdestADD = %Xlate(';':' ':ar4not)
017200120822     c                   end
017300120822      *
017400120822      *  ma se c'è il riferimento destinatario del FIAR5 prende anche
017500120822      *   questo e lo accoda al dato presente su AR4 cercando al meglio
017600120822      *    di dare il più possibile l'informazione.
017700120822     c                   movel     'GEN'         ktrd
017800120822     c                   clear                   DAr5Gen
017900121030     c                   clear                   Referente        31
018000120822     c     kar5          chain     fiar501L
018100120822     c                   if        %Found(fiar501L)
018200120822     c                   Movel     Ar5Uni        DAr5Gen
018300120822     c                   if         §ar5ref <> *blank
018400121010      **  salva il referente
018500121030     c                   eval       Referente = %Xlate(';':' ':§ar5ref)
018600121010      **
018700120822     c                   end
018800120822     c                   end
018900120822      * ----------+
019000120822     C* Al momento non esiste l'informazione in Bartolini
019100120822     C*  Nr Telefono destinatario
019200121030     c                   clear                   NUMtelDEST       18
019300120822     c                   if        §ar5teld <> *blank
019400120822     c                   eval      NUMtelDEST = %Xlate(';':' ':§ar5teld)
019500121016      **
019600121016      **  se c'è una data di richiesta consegna
019700121030     c**********         if        ARBDCR > 0
019800121030     c**********         eval      RAGdestADD = %Trim(RAGdestADD) +
019900121030     c**********                   '/REQ.DEL:' +
020000121030     C**********                   %EDITW(ARBDCR:'    .  .  ')
020100121030     c**********         end
020200121016      **
020300120822     c                   end
020400120822     C*----------------
020500120822     C*  Controllo se ci sono delle note di consegna
020600120822     C*  Reperisco note spedizione
020700120822     C                   MOVEL     '8'           KTRC
020800120822     c                   clear                   NoteCONSEGNA     35
020900120822     C     KAR4          CHAIN     fiar401L                           32
021000120822     C*  (1°campo note)             solo se c'è
021100120822     c                   if        %Found(fiar401L)
021200120822     c                   eval      NoteCONSEGNA = %Xlate(';':' ':ar4not)
021300120822     c                   end
021400121008     c**
021500121008     C* ------------------------------------------------------------------
021600121008     c**  controlla se era un RESO oppure era una bolla generata da ORM
021700121008     c                   eval         Bolla_da_ORM = *blank
021800121008     c                   eval         Bolla_di_RESO= *blank
021900121008      *****
022000121008      **** se bolla di reso
022100121008      ****     x sostituzione riferimento per il Partner che lo richiede
022200121008     c                   if        ARBFBR = 'S'
022300121008     C     KARB          CHAIN     FNlbl01L
022400121008    3C                   IF           %Found(FNlbl01L)
022500121008     c                   eval         Bolla_di_RESO = 'S'
022600121008     c                   end
022700121008     c                   end
022800121008      *****
022900121008      **** se bolla da ORM
023000121008      ****     x sostituzione riferimento per il Partner che lo richiede
023100121008     C                   movel     'M'           KTRC
023200121008     C     KAR4          CHAIN     FiAR401L
023300121008    3C                   IF           %Found(FiAR401L)
023400121008     c                   movel     AR4NOT        dsBl4m
023500121008     c                   if        §B4POE = arbLNA
023600121008     c                   eval         Bolla_da_ORM = 'S'
023700121008     c                   end
023800121008     c                   end
023900121008      *****
024000120822     C* ------------------------------------------------------------------
024100120822     C*  Per ogni Bolla Carica il record del EDGEL00S
024200120822     C*   quindi concatena i dati trimmandoli con (;) separatore
024300120822     C*
024400120822     C                   clear                   edGEL00s
024500120822     C*
024600120822     c                   eval      STYPEREC  ='S'
024700120822     c                   eval      SSNDDEPOT = DepotMittente
024800120822     c                   move      F83DFV        STIMEARR
024900120822     c                   eval      SSNDNAME  = %xLate(';':' ':ARBRSM)
025000120822     c                   eval      SSNDNAMEAD=
025100120822     c                                  %subst(%xLate(';':' ':ARBRSM):28:8)
025200120822     c                   eval      SSNDADDR  = %xLate(';':' ':ARBINM)
025300120822     c                   eval      SSNDCOUNTR= ARBNZM
025400120822     c                   if        SSNDCOUNTR= *blank
025500120822     c                   eval      SSNDCOUNTR= 'IT'
025600120822     c                   end
025700120822     c                   eval      SSNDZIPCOD= ARBCAM
025800120822     c                   eval      SSNDCITY  = %xLate(';':' ':ARBLOM)
025900120822      *
026000120822      * meglio inviare il nostro riferimento Numerico anzichè l'alfabetico
026100120822     c                   clear                   fld15            15
026200120822     c                   move      ARBRMN        fld15
026300120822     C                   movea     fld15         Num
026400120822     C                   do        14            i                 3 0
026500120822     c                   if        Num(i)= '0' or Num(i)= ' '
026600120822     c                   eval      Num(i)=' '
026700120822     c                   else
026800120822     c                   leave
026900120822     c                   end
027000120822     c                   enddo
027100120822     C                   movea     Num           fld15
027200120822     c                   eval      SSNDREFER = %Trim(fld15)
027300120822      *
027400120822     c                   move      F83DFV        SSNDDATE
027500120822     c                   eval      SRCPNAME  = %xLate(';':' ':ARBRSD)
027600120823     c                   eval      SRCPNAMEAD= %xLate(';':' ':RAGdestADD)
027700120822     c                   eval      SRCPADDR  = %xLate(';':' ':ARBIND)
027800120822     c                   eval      SRCPCOUNTR= ARBNZD
027900120822     c                   eval      SRCPZIPCOD= ARBCAD
028000120822     c                   eval      SRCPCITY  = %xLate(';':' ':ARBLOD)
028100120822      *
028200120822     c                   eval      SXXLDIMENS= 'N'
028300120822     c                   z-add     ARBNCL        SNRPARCELS
028400120822     c                   z-add     wpes10        SREALWEIGH
028500121016     c                   z-add     wvol30        SVOLFROMWG
028600120822     c                   eval      SSERVICE  ='O'
028700120822     c                   eval      SAUTOMPOD ='N'
028800120827     c                   eval      SPORTOF   ='F'
028900120822      *
029000120822      *  -----------------------------------------------------------------
029100120823     C*  Scrivo importo da assicurare se c'è   ????????????
029200120822     c                   if        ARBIAS > 0
029300120822     c                   eval      SINSURANCE= ARBIAS
029400120822     C                   end
029500120822      **
029600120823     C*  Se c'è C/Assegno se c'è               ????????????
029700121005     C                   Z-ADD     0             Import_COD       15 3
029800120822     C                   Z-ADD     1             Y                 3 0
029900120822     C     ARBCBO        LOOKUP    C3A(Y)                                 32
030000120822     C     *IN32         IFEQ      '1'
030100120822     C                   MOVEL     D3A(Y)        DS3A
030200120822     C     §3AFCA        IFNE      0
030300120822     C     KARB          CHAIN     FIAR901L
030400120822     c                   if        %Found(FIAR901L) and AR9cas > 0
030500121005      * GEL vuole l'importo moltiplicato 100
030600121005     C     AR9CAS        mult      100           Import_COD
030700120823     c                   z-add     Import_COD    SCODAMOUNT
030800120822     C                   END
030900120822     C                   END
031000120822     C                   END
031100121009      *
031200121009     C*  Attenzione : se si tratta di un reso Nelle NOTE occorre impostare "RETURN"
031300121009     c                   IF           Bolla_di_RESO = 'S'
031400121009      **
031500130607     C                   movel     'E'           KTRCOrigin
031600130607     C                   movel     LBLAAO        KAASOrigin
031700130607     C                   movel     LBLLPO        KLNPOrigin
031800130607     C                   movel     LBLNRO        KNRSOrigin
031900130607     C                   movel     LBLNSO        KNSPOrigin
032000121009     c                   clear                   rifOriginale     30
032100130607     C     KAR4Origin    CHAIN     FiAR401L
032200121009    3C                   IF           %Found(FiAR401L)
032300121009     c                   movel     AR4NOT        dsBl4e
032400121009     c                   eval      rifOriginale = %Xlate(';':' ':§B4ERP)
032500121009     c                   END
032600121009     c                   eval      SNOTE1 = 'RETURN ' + rifOriginale
032700121009      **
032800121009     C                   ELSE
032900121009      **
033000120822     c                   eval      SNOTE1    = NoteConsegna
033100121030      **
033200121030      ** nel campo lungo 50
033300121030      **   il 30/10/2012 si è definito :
033400121030      **    1  1   flag blk/"A" per Appuntamento
033500121030      **    2 19   il numero del contatto telefonico
033600121030      **   20 50   il nome della persona da contattare x la consegna.
033700121030     ***
033800121030      *
033900121030      * Prenotazione su Appuntamento
034000121030     c                   if        ArbTC1 = 'A' or ArbTC2 = 'A'
034100121030     c                   eval      %subst(SNOTE2:1:1) = 'A'
034200121030     c                   end
034300121030      * Contatto telefonico
034400121010     c                   if        numTELdest <> *blank
034500121030     c                   eval      %subst(SNOTE2:2:18) = numTELdest
034600121030     c                   end
034700121030      ** Contatto di consegna
034800121030     c                   if         Referente  <> *blank
034900121030     c                   eval      %subst(SNOTE2:20:31) = Referente
035000121030     c                   end
035100121030      **
035200121016      **  se c'è una data di richiesta consegna
035300121030     c*************      if        ARBDCR > 0
035400121030     c*************      eval      SNOTE2 =  %trim(SNOTE2) + '/REQ.DEL:' +
035500121030     C*************                %EDITW(ARBDCR:'    .  .  ')
035600121030     c*************      end
035700121016      **
035800121009      **
035900121009     C                   END
036000120822      **
036100120822     c                   move      ARBSPE14      SSHPNUMBER
036200120823     c                   eval      SRCPDEPOT = DepotArrivo
036300121009      **
036400121009      * Se si tratta di una bolla generata da ORM
036500121009      **    Bisogna impostare fisso "DE1" nel Depot Ordinante
036600121009     c                   if           Bolla_da_ORM = 'S'
036700121009     c                   eval      SORDEPOT = 'DE1'
036800121009     c                   end
036900121009      *
037000121217      *
037100121217      * per delle PARTICOLARITA' sorte per GEL in questa routine sono messi tutti
037200121217      *  i chiodi da inviare a GEL.
037300121217     C                   exsr      CHIODI
037400121217      *
037500121217      *
037600120822     C* scrittura
037700120822     c                   eval      Dati =
037800120822     c                                   STYPEREC      +  ';'  +
037900120822     c                             %Trim(SSNDDEPOT)    +  ';'  +
038000120822     c                             %Trim(STIMEARR)     +  ';'  +
038100120823     c                             %Trim(%Editc(SSNDCODE:'X')) +  ';'  +
038200120822     c                             %Trim(SSNDNAME)     +  ';'  +
038300120822     c                             %Trim(SSNDNAMEAD)   +  ';'  +
038400120822     c                             %Trim(SSNDADDR)     +  ';'  +
038500120822     c                             %Trim(SSNDCOUNTR)   +  ';'  +
038600120822     c                             %Trim(SSNDZIPCOD)   +  ';'  +
038700120822     c                             %Trim(SSNDCITY)     +  ';'  +
038800120822     c                             %Trim(SSNDREFER)    +  ';'  +
038900120822     c                             %Trim(SSNDDATE)     +  ';'  +
039000120822     c                             %Trim(SRCPNAME)     +  ';'  +
039100120822     c                             %Trim(SRCPNAMEAD)   +  ';'  +
039200120822     c                             %Trim(SRCPADDR)     +  ';'  +
039300120822     c                             %Trim(SRCPCOUNTR)   +  ';'  +
039400120822     c                             %Trim(SRCPZIPCOD)   +  ';'  +
039500120822     c                             %Trim(SRCPCITY)     +  ';'  +
039600120823     c                             %Trim(%Editc(SNRPARCELS:'Z'))+  ';'  +
039700120822     c                             %Trim(SXXLDIMENS)   +  ';'  +
039800120823     c                             %Trim(%Editc(SXXLPARCEL:'Z')) +  ';'  +
039900120823     c                             %Trim(%Editc(SREALWEIGH:'Z')) +  ';'  +
040000120823     c                             %Trim(%Editc(SVOLFROMWG:'Z')) +  ';'  +
040100120822     c                             %Trim(SSERVICE)     +  ';'  +
040200120822     c                             %Trim(SAUTOMPOD)    +  ';'  +
040300120822     c                             %Trim(SSPECIALAD)   +  ';'  +
040400120822     c                             %Trim(STIMELATER)   +  ';'  +
040500120822     c                             %Trim(SPORTOF)      +  ';'  +
040600120823     c                             %Trim(%Editc(SCODAMOUNT:'Z')) +  ';'  +
040700120822     c                             %Trim(SCHEQUE)      +  ';'  +
040800120823     c                             %Trim(%Editc(SINSURANCE:'Z')) +  ';'  +
040900120822     c                             %Trim(SNOTE1)       +  ';'  +
041000120822     c                             %Trim(SNOTE2)       +  ';'  +
041100120823     c                             %Trim(%Editc(SSHPNUMBER:'Z')) +  ';'  +
041200120823     c                             %Trim(SRCPDEPOT)    +  ';'  +
041300121009     c                             %Trim(SORDEPOT)     +  ';'  +
041400120823     c                             %Trim(%Editc(SPICKUPREF:'Z')) +  ';'
041500120822     c**
041600120822     c                   eval      sumDAT = Dati
041700120822     C                   WRITE     RRIFCSUM
041800120822      *
041900120822     c                   endsr
042000120822     C*--------------------------------------------------------------*
042100120822     C* dettaglio spedizione
042200120822     C*--------------------------------------------------------------*
042300120822     C     Dati_ART      BEGSR
042400120822     C*
042500120822     c                   z-add     0             contaColli        3 0
042600120822     C*   Dettaglio Colli
042700120822     C*----------------
042800120822     C     KARB          setll     FNART01L
042900120822     C     KARB          reade     FNART01L
043000120822     C*----------------
043100120822     c                   dow       not %Eof(FNART01L)
043200120822     C*
043300120822     C* aggancia ARS x il segnacollo completo
043400120822     C                   move      'E'           KTRC
043500120822     C     KARS          CHAIN     fIARS01L
043600120822     C*
043700120822     c                   if        %found(fIARS01L)
043800120822     C*  Codice Barcode BRT
043900120822     c                   movel     arsnot        BARCODE18        18
044000120822     C*
044100120822     c                   eval       contaColli =  contaColli + 1
044200120822     C*
044300120822     C* ------------------------------------------------------------------
044400120822     C*  Per ogni COLLO Carica il record del EDGEL00T
044500120822     C*   quindi concatena i dati trimmandoli con (;) separatore
044600120822     C*
044700120823     C                   clear                   edGEL00t
044800120822     C*
044900120822     c                   eval      TTYPEREC  ='T'
045000120822     c                   eval      TSNDDEPOT = DepotMittente
045100120822     c                   move      BARCODE18     TBARCODE
045200120822     c                   move      ARBSPE14      TSHPNUMBER
045300120822     c                   eval      TPACKAGENR=  contaColli
045400120822     c                   eval      TRECDEPOT = DepotArrivo
045500120823     c                   z-add(h)  artPUC        TREALWEIGH
045600120823     c                   z-add(h)  artVUC        TVOLFROMWG
045700120822      ***
045800120822     C* scrittura
045900120822     c                   eval      Dati =
046000120822     c                                   TTYPEREC      +  ';'  +
046100120822     c                             %Trim(TSNDDEPOT)    +  ';'  +
046200120823     c                             %Trim(TBARCODE)     +  ';'  +
046300120823     c                             %Trim(%Editc(TSHPNUMBER:'Z'))+  ';'  +
046400120822     c                             %Trim(TEMPTY1)      +  ';'  +
046500120822     c                             %Trim(TEMPTY2)      +  ';'  +
046600120823     c                             %Trim(%Editc(TEMPTY3   :'Z'))+  ';'  +
046700120823     c                             %Trim(%Editc(TEMPTY4   :'Z'))+  ';'  +
046800120822     c                             %Trim(TEMPTY5)      +  ';'  +
046900120823     c                             %Trim(%Editc(TPACKAGENR :'Z'))+  ';'  +
047000120822     c                             %Trim(TRECDEPOT)    +  ';'  +
047100120823     c                             %Trim(%Editc(TLENGHTCM :'Z')) +  ';'  +
047200120823     c                             %Trim(%Editc(TWIDTHCM  :'Z')) +  ';'  +
047300120823     c                             %Trim(%Editc(THEIGHTCM :'Z')) +  ';'  +
047400120823     c                             %Trim(%Editc(TREALWEIGH:'Z')) +  ';'  +
047500120823     c                             %Trim(%Editc(TVOLFROMWG:'Z')) +  ';'
047600120822     c**
047700120822     c                   eval      sumDAT = Dati
047800120822     C                   WRITE     RRIFCSUM
047900120822     c                   end
048000120822      *
048100120822     C     KARB          reade     FNART01L                               41
048200120822     c                   enddo
048300120822     C*----------------
048400120822     C*
048500020220      *
048600020226      *
048700020219     c                   endsr
048800950428     C*****************************************************************
048900950428     C* OPERAZIONI INIZIALI
049000950428     C*****************************************************************
049100950428     C     *INZSR        BEGSR
049200010426     C*
049300960530     C* Definisco chiavi di accesso
049400950428     C     KARB          KLIST
049500960530     C                   KFLD                    KAAS
049600960530     C                   KFLD                    KLNP
049700960530     C                   KFLD                    KNRS
049800960530     C                   KFLD                    KNSP
049900020227      *
050000950428     C     KAR4          KLIST
050100960530     C                   KFLD                    KAAS
050200960530     C                   KFLD                    KLNP
050300960530     C                   KFLD                    KNRS
050400960530     C                   KFLD                    KNSP
050500960530     C                   KFLD                    KTRC
050600130607      *
050700130607     C     KAR4Origin    KLIST
050800130607     C                   KFLD                    KAASOrigin
050900130607     C                   KFLD                    KLNPOrigin
051000130607     C                   KFLD                    KNRSOrigin
051100130607     C                   KFLD                    KNSPOrigin
051200130607     C                   KFLD                    KTRCOrigin
051300031210      *
051400031210     C     KAR5          KLIST
051500031210     C                   KFLD                    KAAS
051600031210     C                   KFLD                    KLNP
051700031210     C                   KFLD                    KNRS
051800031210     C                   KFLD                    KNSP
051900031210     C                   KFLD                    KTRD              3
052000020227      *
052100020226     C     KARS          KLIST
052200020226     C                   KFLD                    artfls
052300020226     C                   KFLD                    artlna
052400020226     C                   KFLD                    artnrs
052500120822     C                   KFLD                    artnsc
052600020226     C                   KFLD                    KTRC
052700020227      *
052800960530     C     KTAB          KLIST
052900960530     C                   KFLD                    KKUT
053000960530     C                   KFLD                    KCOD
053100020227      *
053200961126     C     KTAB1         KLIST
053300961126     C                   KFLD                    KKUT
053400961126     C                   KFLD                    KCOD
053500961126     C                   KFLD                    KKEY
053600020227      *
053700020227      *------------
053800960530     C*  Definisco variabili
053900960530     C     *LIKE         DEFINE    ARBAAS        KAAS
054000960530     C     *LIKE         DEFINE    ARBLNP        KLNP
054100960530     C     *LIKE         DEFINE    ARBNRS        KNRS
054200960530     C     *LIKE         DEFINE    ARBNSP        KNSP
054300960530     C     *LIKE         DEFINE    AR4TRC        KTRC
054400130607     C     *LIKE         DEFINE    ARBAAS        KAASOrigin
054500130607     C     *LIKE         DEFINE    ARBLNP        KLNPOrigin
054600130607     C     *LIKE         DEFINE    ARBNRS        KNRSOrigin
054700130607     C     *LIKE         DEFINE    ARBNSP        KNSPOrigin
054800130607     C     *LIKE         DEFINE    AR4TRC        KTRCOrigin
054900960530     C     *LIKE         DEFINE    TBLKUT        KKUT
055000960530     C     *LIKE         DEFINE    TBLCOD        KCOD
055100961126     C     *LIKE         DEFINE    TBLKEY        KKEY
055200020220     c     *like         define    arbRSD        fldRSD
055300020220     c     *like         define    ar4not        fldnot
055400020220     c     *like         define    arbind        fldind
055500020220     c     *like         define    arbLOD        fldLOD
055600030521     C     *LIKE         DEFINE    §qttpc        WTARA
055700020227      *------------
055800020220     C* Inizializzo variabili totale
055900960530     C* Imposto data del giorno
056000960530     C                   TIME                    W0140            14 0
056100960530     C                   MOVE      W0140         G02DAT            8 0
056200960530     C                   MOVEL     *BLANK        G02ERR
056300960530     C                   CALL      'XSRDA8'
056400960530     C                   PARM                    WLBDA8
056500960530     C                   MOVEL     G02INV        WOGGI             8 0
056600110512      *
056700120305      *
056800030521     C** CARICAMENTO DATI VARI TASSAZIONE
056900030521     C                   Z-ADD     1             KKUT
057000030521     C                   MOVEL     'QT'          kcod
057100030521     C                   MOVEL     *BLANKS       kKEY
057200030521     C                   MOVEL     '1'           kKEY
057300030521     C     KTAB1         CHAIN     TABEL00F                           30
057400030521     C  N30              MOVEL     TBLUNI        DSQT1
057500030521     C*
057600010424      *------------
057700960530     C* Caricamento Tabella codici bolla
057800960530     C                   Z-ADD     0             X                 3 0
057900960530     C                   MOVEL     '3A'          KCOD
058000960530     C                   Z-ADD     1             KKUT
058100960530     C     KTAB          CHAIN     TABEL00F                           30
058200960530     C     *IN30         DOWEQ     '0'
058300960530     C     TBLFLG        IFEQ      *BLANKS
058400960530     C                   ADD       1             X
058500960530     C                   MOVEL     TBLKEY        C3A(X)
058600960530     C                   MOVEL     TBLUNI        D3A(X)
058700960530     C                   END
058800960530     C     KTAB          READE     TABEL00F                               30
058900960530     C                   END
059000020227      *------------
059100961126     C* Reperisco Kg per metro cubo per calcolo peso tassabile
059200961126     C                   MOVEL     '7A'          KCOD
059300961126     C                   MOVEL(P)  '2'           KKEY
059400961126     C     KTAB1         CHAIN     TABEL00F                           31
059500961126     C  N31              MOVEL     TBLUNI        DS7A2
059600961126     C   31              CLEAR                   DS7A2
059700960530     C*
059800960530     C                   ENDSR
059900020227     C*****************************************************************
060000030317     C*  Reperisce Peso/Volume o da CML o da Bollettato
060100030317     C*****************************************************************
060200030317     C     VOL_PES       BEGSR
060300030317     C*
060400030317     c     *like         define    arbvlb        wvol
060500030317     c     *like         define    arbpkb        wpes
060600121016     c     *like         define    arbpkb        wvol30           +3
060700121001     c     *like         define    arbpkb        wpes10           +1
060800150825      **
060900030317      * Prima cerca il volume fra bollettato e CML
061000150825     C     arbncR        ifeq      arbncl
061100030317      * se è considerata un'unica spedizione
061200150825     C                   z-add     ARBVLC        WVOL
061300150825     C                   else
061400150825      **
061500150825     C     ARBVLC        ifge      ARBVLb
061600150825     C                   z-add     ARBVLC        WVOL
061700150825     C                   else
061800150825     C                   z-add     ARBVLb        WVOL
061900150825     C                   end
062000150825      **
062100150825     C                   endif
062200150825      *-------
062300150825      * Anche GEL è stata allineata a tutti i programmi di EDI x quel che riguarda
062400150825      * il volume.
062500150825      *-------
062600150825      * //Poichè il Volume x GEL serve per creare il peso volumato
062700150825      * //sul quale poi GEL ci fatturerà la spedizione, NON si deve utlizzare
062800150825      * //la regola sopra descritta e asteriscata bensì si deve solo prendere
062900150825      * //il Volume da Fatturare  che poi sarà moltiplicato per il coefficiente
063000150825      * //di conversione x ottenere il peso come da tariffa.
063100150825     C**********         z-add     ARBVLf        WVOL
063200150825      *-------
063300030317      *
063400030521     C* PESO VDL - TARA
063500030521     C     §QTTPC        MULT      arbncp        WTARA
063600030521     C                   Z-ADD     arbPKC        wpes
063700030521     c                   sub(h)    wtara         wpes
063800030521      *
063900030317      * Poi cerca il peso fra bollettato e CML
064000050112     C     arbncP        ifeq      arbncl
064100030317      * se è considerata un'unica spedizione
064200030317     C                   ELSE
064300030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
064400030521     C     wpes          iflt      arbPKb
064500030317     C                   Z-ADD     arbPKb        wpes
064600030317     C                   end
064700030317     C*
064800030317     C                   ENDIF
064900030317     C*
065000030317     C                   ENDSR
065100030521     C*-----------------------------------------------------*
065200121217     C*    PARTICOLARITA' come CHIODI
065300121217     C*-----------------------------------------------------*
065400121217     C     CHIODI        BEGSR
065500121217      *
065600121217      *  Per le SPEDIZIONI di ELECTROLUX la consegna è "M" entro le 12:00
065700121217      *    con il cliente mittente 1160985
065800121220     c                   if        arbKSC = 1160985
065900121217     c                   eval      SSERVICE  ='M'
066000121217     c                   end
066100121217      *
066200121217     C                   ENDSR
066300121217     C*-----------------------------------------------------*
