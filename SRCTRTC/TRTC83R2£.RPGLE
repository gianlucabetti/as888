000100960530      ***********************************************************************
000200031209      *   Attivato x 2004 TSR particolari.  TSR+++3                         *
000300150722      * ATTENZIONE GWEISS   NON si deve passare la Data Consegna Richiesta  *
000400150722      *                     tranne per il cliente KIKO 0934683 (CHIODO)     *
000500120120      ***********************************************************************
000600120120      *  Ducros : vuole un altro tipo di riferimento al posto del RFF+CU
000700120120      *   in questo riferimento RFF+ABO:xxxxxxxxxx deve essere passato il
000800120120      *   riferimento dell'ORM che avevano richiesto, oppure,
000900120120      *   trattandosi di un reso, il loro riferimento presente sulla bolla
001000120120      *   di import.  Questa funzione è attivata tramite la tabella PT
001100120120      *   in modo da poterla utilizzare anche su altri Partner futuri. 16/1/2012
001200020402      ***********************************************************************
001300020402      *   ESPORTA DATI BOLLE VERSO  E.D.I. ESTERO                           *
001400960530      ***********************************************************************
001500891004     H DECEDIT('0,') DATEDIT(*DMY/)
001600000207      *---------------------------------------------------------------------*
001700970204     FFNARB01L  UF   E           K DISK    COMMIT
001800060213     Ffiar401L  IF   E           K DISK
001900031210     Ffiar501L  IF   E           K DISK
002000090223     Ffiar601L  IF   E           K DISK
002100051114     FFIAR901L  IF   E           K DISK
002200960530     FFNART01L  IF   E           K DISK
002300051017     FFIARS01L  IF   E           K DISK
002400970203     FFNLBL02L  IF   E           K DISK
002500120119     FFNLBL01L  IF   E           K DISK    rename(fnLBL000:fnLBL01)
002600120119     F                                     prefix(BOL:3)
002700120119     FFNorm01L  IF   E           K DISK
002800050309     FTIGCP02L  IF   E           K DISK
002900960726     FEDTAB01L  UF   E           K DISK
003000960530     FTABEL00F  IF   E           K DISK
003100960530     FEDIFCSUM  UF A E             DISK
003200000207      *---------------------------------------------------------------------*
003300000207      * Schiere
003400000207      *---------------------------------------------------------------------*
003500000207      * Schiera per reperimento importo/valuta tipo/qualif.imp.
003600000207      * Schiera per reperimento testo libero singola sped.
003700000207      * Schiera per reperimento nr.riferimento spedizione
003800000207      * Schiera per reperimento persona a cui è diretta comunic.
003900000207      * Schiera per reperimento nr.seganacollo
004000000207      * Priorità trasporto
004100960530     D PTR             S              3    DIM(100)
004200960530     D TSP             S              1    DIM(100)
004300000207      * Termini di consegna
004400960530     D CTB             S              1    DIM(100)
004500140428     D CTBkey          S              3    DIM(100)                             ctb+suffisso
004600960712     D TCO             S              3    DIM(100)
004700000207      * Codici bolla
004800960530     D C3A             S              2    DIM(200)
004900960530     D D3A             S             89    DIM(200)
005000000207      * Codici ISO nazioni x E.D.I.
005100011217     D C15             S              3    DIM(500)
005200011217     D ISO             S              3    DIM(500)
005300170418     D PRF             S              4    DIM(500)
005400000207      * Tipo pagamento C/Assegno
005500130227     D C1A             S             10    DIM(100)
005600960716     D TPI             S              2    DIM(100)
005700130215     D TPIkey          S             35    DIM(100)
005800000327      * Codici ISO nazioni x E.D.I.
005900000327     D CFF             S              7  0 DIM(100)
006000000327     D DFF             S             35    DIM(100)
006100010424     D* Tabella partner
006200121105     D CPT             S             35    DIM(200)
006300121105     D DPT             S             90    DIM(%elem(CPT))
006400121105     D KSC             S              7  0 DIM(%elem(CPT))
006500121105     D DPU             S             90    DIM(%elem(CPT))
006600121105     D DPv             S             90    DIM(%elem(CPT))
006700121105     D DPz             S             90    DIM(%elem(CPT))
006800170412     D DPs             S             90    DIM(%elem(CPT))
006900121105     D TRUL0SDS      E DS
007000150825     D DSQT1         E DS                  INZ
007100000207      *---------------------------------------------------------------------*
007200000207      * DS
007300000207      *---------------------------------------------------------------------*
007400000207      * .. per scompozione dati ricevuti a seconda del tipo record
007500960530     D EDTD00        E DS                  EXTNAME(EDTD00DS)
007600000207     D  D00                   77    142    DIM(3)
007700960530     D EDTD05        E DS                  EXTNAME(EDTD05DS)
007800000207     D  D05                   13    362    DIM(5)
007900000207     D  D05A                 366    715    DIM(5)
008000960530     D EDTD10        E DS                  EXTNAME(EDTD10DS)
008100000207     D  D10                   73    186    DIM(3)
008200960530     D EDTD15        E DS                  EXTNAME(EDTD15DS)
008300000207     D  D15                  445    500    DIM(2)
008400960530     D EDTD20        E DS                  EXTNAME(EDTD20DS)
008500960530     D EDTD25        E DS                  EXTNAME(EDTD25DS)
008600960530     D EDTD30        E DS                  EXTNAME(EDTD30DS)
008700000207     D  D30                   10    709    DIM(20)
008800960530     D EDTD35        E DS                  EXTNAME(EDTD35DS)
008900960530     D EDTD40        E DS                  EXTNAME(EDTD40DS)
009000010424      *-------
009100010424      *  Ds per reperimento tabella PT e campo risultato
009200010424     D DSPT          E DS                  EXTNAME(EDIDSPT)
009300010424     D Partner         S             35
009400010730     D DSPU          E DS                  EXTNAME(EDIDSPU)
009500120119     D DSPV          E DS                  EXTNAME(EDIDSPV)
009600120705     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
009700170412     D DSPS          E DS                  EXTNAME(EDIDSPS)
009800010424      *-------
009900000207      *  Ds per reperimento dati importi
010000960530     D WTD00           DS
010100960530     D  WTPI                   1      3
010200960530     D  WIMP                   4     19  3
010300960530     D  WVAL                  20     22
010400000207      *  Ds per reperimento riferimeti singola spedizione
010500960530     D WTD10           DS
010600960530     D  WTPR                   1      3
010700960530     D  WRIF                   4     38
010800000207      * DS per reperimento destinatario comunicazione
010900960530     D WTD15           DS
011000960530     D  WTCD                   1      3
011100960530     D  WCMD                   4     28
011200000207      *---------------------------------------------------------------------*
011300000207      * .. per reperimento tipo record di EDIFTMIN letto
011400000207      *                    .. 5 - 8  : tipo record letto
011500960705     D WDAT            DS          1950
011600960726     D  SUMPRG                 1      4  0
011700960530     D  SUMTPR                 5      8
011800000207      *---------------------------------------------------------------------*
011900000327     D DSFF          E DS                  EXTNAME(EDIDSFF)
012000000327     D DSMS          E DS                  EXTNAME(EDIDSMS)
012100960530     D TRTC82        E DS                  EXTNAME(TRTC82DS)
012200060622     D dsBL4i        E DS
012300090223     D dsBL4a        E DS
012400120119     D dsBL4m        E DS
012500120119     D dsBL4e        E DS
012600960530     D DS3A          E DS
012700961126     D DS7A2         E DS
012800960822     D DS15          E DS
012900960530     D DSCV          E DS
013000031210     D dar5gen       e ds
013100170418     D dar5emd       e ds
013200170418     d  fldSMS         s                   like(§ar5TEL)
013300170418     d  fldPREF        s              4A
013400170419     d  Invia_alert_mail...
013500170419     d                 s                   like(§ar5EML)
013600170419     d  Invia_alert_sms...
013700170419     d                 s             30A
013800950428     D KPJBA         E DS
013900121112     d kpjbusav        S                   like(Kpjbu)
014000960530     D WLBDA8          DS                  INZ
014100950428     D  G02DAT                 1      8  0
014200950428     D  G02INV                 9     16  0
014300950428     D  G02ERR                17     17
014400950428     D  G02TGI                18     22  0
014500960530     D*  DS x scomposizione numero spedizione
014600960530     D DSSPE           DS
014700960530     D  ARBAAS                 1      4  0
014800960530     D  ARBLNP                 5      7  0
014900960530     D  ARBNRS                 8      9  0
015000960530     D  ARBNSP                10     16  0
015100960905     D  SPE01                  3     16  0
015200960530     D*  DS x scomposizione segnacollo
015300960530     D DSSGN           DS
015400961029     D  ARTFLS                 1      3  0
015500960530     D  ARTLNA                 4      6  0
015600960530     D  ARTNRS                 7      8  0
015700960530     D  ARTNSC                 9     15  0
015800960829     D  ARTZNC                16     17  0
015900970203     D                 DS
016000970203     D  TESTO1                 1      5
016100970203     D  GCAAGC                 6      7  0
016200970203     D  GCAFGC                 9     11  0
016300970203     D  GCANGC                13     19  0
016400970203     D  TESTO2                21     25
016500970203     D  VARIO                 27     33
016600970203     D  RIFGIA                 1     33
016700970203     D                 DS
016800970203     D  TESTO3                 1      8
016900970203     D  RAAS                   9     10  0
017000970203     D  RLNP                  12     14  0
017100970203     D  RNRS                  16     17  0
017200970203     D  RNSP                  19     25  0
017300970203     D  RIFSPE                 1     25
017400001222     D                 DS
017500001222     D  chkSPE                 9     25
017600001222     D  dgtSPE                 1     25
017700050518      **
017800050518     d  fld15a         s             15
017900050518     D  a15            s              1    DIM(15)
018000120119      **------
018100120119     D Richiesta_sostituzione_CU...
018200120119     d                 s              1
018300120119     D Bolla_di_Reso...
018400120119     d                 s              1
018500120119     D Bolla_da_ORM...
018600120119     d                 s              1
018700120119     D Rif_RESO_Orig...
018800120119     d                 s             35
018900120119     D Rif_ORM_Orig...
019000120119     d                 s             35
019100120119     d  Qualificatore_RESO...
019200120119     d                 s              3
019300120119     d  Qualificatore_ORM...
019400120119     d                 s              3
019500120119     d  Tipo_Rif_RESO...
019600120119     d                 s              1
019700120119     d  Tipo_RIf_ORM...
019800120119     d                 s              1
019900120330     d  Qualificatore_PCI...
020000120330     d                 s              3
020100120330     d  Quanti_x_PCI...
020200120330     d                 s              2s 0
020300120119     d  Num            s              3s 0
020400120119      **------
020500050518      **
020600970203     D*  Definisco costanti per bolle reso
020700970203     D COST1           C                   CONST('EX SPED.')
020800970203     D COST2           C                   CONST('GIAC.')
020900020711     D COST3           C                   CONST('LNP.')
021000001222     D digits          C                   CONST('0123456789')
021100150722     D GWEISS          C                   CONST('GW-SPAC                      -
021200150722     D                                           ')
021300060116     D Pracht          C                   CONST('PRACHT                       -
021400060116     D                                           ')
021500970124     I*
021600970204     I***                 CMD     1   1 56               OVRDBF
021700960530     I*---------------------------------------------------------------------*
021800960530     I* FILE
021900960530     I*---------------------------------------------------------------------*
022000960704     IRRIFCSUM
022100960530     I              RRIFCSUM                    SUMDAT
022200960530     C*---------------------------------------------------------------------*
022300960530     C* Ciclo principale                                                   -*
022400960530     C*---------------------------------------------------------------------*
022500010426     C     *ENTRY        PLIST
022600010426     C                   PARM                    KPJBA
022700010426     C                   MOVEL     KPJBU         TRTC82
022800010426     C*
022900960530     C*  Se c'è numero spedizione scrivo dettaglio
023000970124    2C     D82NSP        IFNE      0
023100010426     C*
023200010426     C*-------------------
023300010426     C*  In base al codice del PARTNER reperisco identificativo
023400010426     C*   per decidere per chi scrivere il VOLUME subito successivo.
023500010426     C*    da utilizzare nella scrittura del TD25 dettaglio Volume.
023600010426     C                   Z-ADD     1             XX                3 0
023700010426     C                   Clear                   partner
023800010426     C     D82CPT        LOOKUP    KSC(XX)                                32
023900010426     C   32              MOVEL     CPT(XX)       partner
024000010730     C   32              MOVEL     DPU(XX)       DSPU
024100120119     C   32              MOVEL     DPV(XX)       DSPV
024200120705     C   32              MOVEL     DPZ(XX)       DSPZ
024300170412     C   32              MOVEL     DPs(XX)       DSPS
024400170412      **
024500120119     c                   eval       Richiesta_sostituzione_CU =' '
024600170412      **
024700120119      * imposta richiesta sostituzione RFF+CU con altro qualificatore
024800120119      *   per invio riferimento originale di un RESO o di un ORM.
024900120119     c                   if        §PVCURESO ='S' or §PVCUORM ='S'
025000120119     c                   eval       Richiesta_sostituzione_CU ='S'
025100120119     C                   eval         Qualificatore_RESO = §PVCUQUAR
025200120119     C                   eval         Qualificatore_ORM  = §PVCUQUAO
025300120119     C                   eval              Tipo_Rif_RESO = §PVCUTIPR
025400120119     C                   eval              Tipo_RIf_ORM  = §PVCUTIPO
025500120119     c                   endif
025600010426     C*-------------------
025700120330      ** aggiunti x SEUR  sul segm. PCI il qualificatore da Inviare e quanti
025800120330      ** Barcode per segmento PCI devono essere inviati
025900120330     C                   eval         Qualificatore_PCI  = §PVquaPCI
026000120330      *
026100120330      * inizializza a 20 la schiera dei codici a barre
026200120330     C                   z-add     20            Quanti_x_PCI
026300120402     c                   if            §PVpciXseg <> *blank and
026400120402     c                                 §PVpciXseg <> *zeros
026500120330     C                   move      §PVpciXseg    Quanti_x_PCI
026600120330     c                   end
026700120330     C*-------------------
026800960530     C*  Eseguo posizionamento su archivio bolle per passaggio param.
026900960530     C                   Z-ADD     D82AAS        KAAS
027000960530     C                   Z-ADD     D82LNP        KLNP
027100960530     C                   Z-ADD     D82NRS        KNRS
027200960530     C                   Z-ADD     D82NSP        KNSP
027300960530     C     KARB          CHAIN     FNARB01L                           31
027400970124    3C     *IN31         IFEQ      *OFF
027500120119      *
027600120119     c                   eval         Bolla_da_ORM = *blank
027700120119     c                   eval         Bolla_di_RESO= *blank
027800120119      *****
027900120119      **** se bolla di reso
028000120119      ****     x sostituzione riferimento per il Partner che lo richiede
028100120119     c                   if        ARBFBR = 'S'  and §PVCURESO ='S'
028200120119     C     KARB          CHAIN     FNlbl01L
028300120119    3C                   IF           %Found(FNlbl01L)
028400120119     c                   eval         Bolla_di_RESO = 'S'
028500120119     c                   end
028600120119     c                   end
028700120119      *****
028800120119      **** se bolla da ORM
028900120119      ****     x sostituzione riferimento per il Partner che lo richiede
029000120119     c                   if         §PVCUORM ='S'
029100120119     C                   movel     'M'           KTRC
029200120119     C     KAR4          CHAIN     FiAR401L
029300120119    3C                   IF           %Found(FiAR401L)
029400120119     c                   movel     AR4NOT        dsBl4m
029500120119     c                   if        §B4POE = arbLNA
029600120119     c                   eval         Bolla_da_ORM = 'S'
029700120119     c                   end
029800120119     c                   end
029900120119     c                   end
030000170418      **-----------
030100170418      **   abilitato all'invio dell'Alert (Predict) Email o SMS
030200170418     c                   eval      Invia_alert_mail = ' '
030300170418     c                   eval      Invia_alert_sms  = ' '
030400170412      **
030500170418      **  x inviare l'Alert  (tab.PS)
030600170418     c                   if         §PSSALERT ='S'
030700170419     c                   clear                   fldPREF
030800170419     c                   clear                   fldSMS
030900170418      *-
031000170418     c                   movel     'EMD'         ktrd
031100170418     c                   clear                   DAr5emd
031200170418      *  EMAIL o SMS - come servizio di Alert di consegna -
031300170418     c     kar5          chain     fiar501L
031400170418     c                   if        %Found(fiar501L)
031500170418     c                   Movel     Ar5Uni        DAr5emd
031600170419     c                   eval      Invia_alert_mail = §ar5EML
031700170419     c                   movel     §ar5TEL       input16          16
031800170418     c                   exsr      compatta_NR
031900170418     c                   movel     out16         fldSMS
032000170418      * se c'è già il prefisso
032100170418     c                   if        %subst(fldSMS:1:1) = '+' or
032200170418     c                             %subst(fldSMS:1:2) = '00'
032300170419     c                   eval      Invia_alert_sms    = fldSMS
032400170418     c                   else
032500170418      *  altrimenti con la
032600170418     C* Nazione Destinatario imposta il Prefisso nel numero
032700170418     C                   Z-ADD     1             YY
032800170418     C     arbNZD        LOOKUP    C15(YY)                                32
032900170418     C  N32D82NAZ        LOOKUP    C15(YY)                                32
033000170418      *   se c'è il pref.internazionale in tabella Nazioni
033100170418      *   imposta il codice preceduto dal '+'
033200170418     c   32              if        PRF(YY) <> *blank
033300170418     C                   MOVEL     PRF(YY)       fldPREF
033400170418     c                   end
033500170418      *
033600170418      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
033700170418     c                   if        fldPREF <> *blank and fldSMS <> *blank
033800170419     c                   eval       Invia_alert_sms = '+'
033900170418     c                               + %Trim(fldPREF) + %Trim(fldSMS)
034000170418     c                   end
034100170418     c                   endif
034200170418     c                   end
034300170418      *-
034400170418     c                   endIF
034500170418     C*----------------
034600170418      *
034700960530     C*  Scrittura D00
034800960530     C                   EXSR      WRTD00
034900960530     C*  Scrittura D05
035000960530     C                   EXSR      WRTD05
035100170419     C*  Scrittura D05 x Alert
035200170419     c                   if          invia_alert_SMS  <> *blank  or
035300170419     c                               invia_alert_Mail <> *blank
035400170419     C                   EXSR      WRTD05_alert
035500170419     c                   end
035600960530     C*  Scrittura D10
035700960530     C                   EXSR      WRTD10
035800960530     C*  Scrittura D15
035900960530     C                   EXSR      WRTD15
036000960530     C*  Scrittura D20
036100960530     C                   EXSR      WRTD20
036200960530     C*  Scrittura D25
036300960530     C                   EXSR      WRTD25
036400960827     C*  Scrittura D30 solo se lo vogliono
036500980408    4C     D82DET        IFNE      *BLANKS
036600960530     C                   EXSR      WRTD30
036700970124    4C                   END
036800960827     C*
036900970124    3C                   END
037000960530     C*  Fleggo FNARB come trasmesso
037100960530     C                   MOVEL     'T'           ARBFT3
037200961126     C                   Z-ADD     D82DFV        ARBDT3
037300960530     C                   EXCEPT    AGGTRA
037400970124     C                   SETON                                        RT
037500970124     C*
037600970124   X2C                   ELSE
037700060116     C*
037800960530     C*  Se non c'è spedizione: ultima volta --> Scrivo totali in DS
037900960726     C                   Z-ADD     §MSNUM        D82PRG
038000960530     C                   Z-ADD     WTONCL        D82TNC
038100960530     C                   Z-ADD     WTOPKF        D82TPK
038200960530     C                   Z-ADD     WTOVLF        D82TVL
038300960530     C                   Z-ADD     WPRG          D82TSP
038400960705     C                   MOVEL     TRTC82        KPJBU
038500060116      **
038600060116 ?    **  Il programma se ha fatto l'EDI x la PRACHT incrementa di altri 3 numeri il
038700060116 ?    **   numeratore x un totale di 4 incrementi(considerando anche quello fatto nella
038800060116 ?    **   INZSR)e aggiorna la tabella numeratori x un manifest di altro PARTNER.
038900060116 ?    **  Questo accade perchè il msg della PRACHT deve essere diviso in 4 EDI separati.
039000060116 ?    **   La numerazione dei msg PRACHT viene rivista nei pgm seguenti x l'invio dell'EDI.
039100060116      **
039200060116     c                   if        Partner = PRACHT
039300060116      **
039400060116 ?    **  Aggiornamento NUMERATORE solo x PRACHT
039500150722      **   poichè aveva 4 messaggi separati.
039600060116     C                   Z-ADD     1             §MSNUM
039700060116     C                   MOVEL     'MS'          KCOD1
039800060116     C                   MOVEL(P)  '785'         KKEY1
039900060116     C     KTABE         CHAIN     EDTAB01L
040000060116     C                   if        %found and TABFLG = *BLANKS
040100060116     C                   MOVEL     TABUNI        DSMS
040200060116     C                   ADD       3             §MSNUM
040300060116     C                   Z-ADD     WOGGI         §MSDAT
040400060116     C                   MOVEL     DSMS          TABUNI
040500060116     C                   UPDATE    EDTAB000
040600060116     C                   END
040700060116      **
040800060116     c                   end
040900060116      **
041000970124     C                   SETON                                        LR
041100970124    2C                   END
041200960530     C*--------------------------------------------------------------*
041300960530     C* WRTD00: scirvo tipo record D00 in IFCSUM                    -*
041400960530     C*--------------------------------------------------------------*
041500960530     C     WRTD00        BEGSR
041600960530     C*
041700960530     C                   CLEAR                   EDTD00
041800960702     C                   CLEAR                   AR9TIC
041900960702     C                   CLEAR                   AR9CAS
042000960530     C*  Imposto nr.spedizione
042100960530     C                   ADD       1             WPRG              7 0
042200960530     C                   Z-ADD     WPRG          DA1490
042300960906     C     D82CNI        IFLE      15
042400960905     C     D82SEC        ANDEQ     'N'
042500960905     C                   MOVEL     SPE01         DA1004
042600960905     C                   ELSE
042700960905     C                   MOVEL     DSSPE         DA1004
042800960905     C                   END
042900960530     C                   Z-ADD     9999          DA1312
043000150722      ***
043100150722      * chiodo:
043200150722      *** ??????? ---------------------------------------------------
043300150722      ***  CHIODO per GWEISS la Data Consegna Richiesta deve essere inviata
043400150722      *** ???????     solo per il cliente KIKO 0934683  -  Altrimenti NO
043500150722      ***  *xché la sped.costerebbe il 30% in più*
043600150722      ***     (Solfrini - Santini - Bocchi - Delledonne)
043700150722     C     Partner       IFeq      GWEISS
043800150722     c                   if          ARBKSC <> 934683
043900150722     c                   z-add     0             arbdcr
044000150722     c                   end
044100150722     c                   end
044200150722      *** ??????? ---------------------------------------------------
044300150722      ***
044400960530     C*  Se c'è scrivo data consegna richiesta
044500960530     C     ARBDCR        IFNE      0
044600960530     C                   MOVEL     'Z73'         DA2005
044700960530     C                   MOVEL     ARBDCR        DA2380
044800960530     C                   MOVEL     '102'         DA2379
044900960530     C                   END
045000031209      *
045100960530     C*  Recupero il tipo spedizione dal tipo servizio
045200960726     C**                   Z-ADD1         X
045300960726     C**         ARBTSP    LOKUPTSP,X                    32
045400960726     C**         *IN32     IFEQ '1'
045500960726     C**                   MOVELPTR,X     DA4219
045600960726     C**                   END
045700031209      *
045800020123      *
045900031209      * ? Particolarità TSR da attivare dal 1.01.2004
046000031209      *  -----------------------------------------------------------------
046100031209      *  Pre-noon serv -> Espresso                        TSR+++3+PRN
046200031209      *  Envelopes     -> busta di plastica 1 Kg.         TSR+++3+ENV
046300031209      *  Booking ins   -> Appuntamento (+ data DTM Z73)   TSR+++3+BKI
046400031209      *  Saturday serv -> Consegna di sabato (non util.)  TSR+++3+SPS
046500120705      *  GoodWaitCollect> FERMO DEPOSITO se abilitato PT  TSR+++3+GWC
046600031209      *  -----------------------------------------------------------------
046700031209      * Espresso consegna prima di mezzogiorno
046800091014     c                   if        ArbTSP = 'E'  or
046900091014     c                             ArbTSP = 'H'
047000031209     C                   eval      DA4219 = '3  '
047100031209     C                   eval      DA7085 = 'PRN'
047200031209     c                   end
047300031209      *
047400031209      * Prenotazione su Appuntamento
047500031209      *  + data prevista consegna comunque precedentemente impostata
047600031209     c                   if        ArbTC1 = 'A' or  ArbTC2 = 'A'
047700031209     C                   eval      DA4219 = '3  '
047800031209     C                   eval      DA7085 = 'BKI'
047900031209     c                   end
048000031209      *
048100031209      * Servizio di Sabato   non è gestito da Bartolini
048200031209     c*******            if        ...............
048300031209     C*******            eval      DA4219 = '3  '
048400031209     C*******            eval      DA7085 = 'SPS'
048500031209     c*******            end
048600031209      *
048700031209      * Busta di plastica con max 1Kg. non è gestita da Bartolini
048800031209     c*******            if        ...............
048900031209     C*******            eval      DA4219 = '3  '
049000031209     C*******            eval      DA7085 = 'ENV'
049100031209     c*******            end
049200031209      *
049300120705      *  FERMO DEPOSITO
049400120705     c                   if        ARBFFD = 'S' and §pzTSRgwc ='S'
049500120705     C                   eval      DA4219 = '3  '
049600120705     C                   eval      DA7085 = 'GWC'
049700120705     c                   end
049800120705      *
049900031209      *  -----------------------------------------------------------------
050000031209      *
050100960530     C*  Scrivo valore merce se c'è
050200970219     C                   Z-ADD     0             X
050300021118     C                   if        ARBVMD > 0
050400021118     C                   ADD       1             X
050500021118     C                   MOVEL     '77 '         WTPI
050600021118     C                   Z-ADD     ARBVMD        WIMP
050700960530     C*  Se divisa a blanks imposto italiana
050800021118     C                   if        ARBVAD = *BLANKS
050900021118     C                   MOVEL     WDIVIT        WVAL
051000021118     C                   else
051100021118     C                   MOVEL     ARBVAD        WVAL
051200021118     C                   end
051300021118     C                   MOVEL     WTD00         D00(X)
051400021118     C                   end
051500021118      *
051600960530     C*  Scrivo importo da assicurare se c'è
051700961115     C***        ARBIAS    IFGT 0
051800961115     C***                  ADD  1         X
051900961115     C***                  MOVEL'157'     WTPI
052000961115     C***                  Z-ADDARBIAS    WIMP
052100960530     C*  Se divisa a blanks imposto italiana
052200961115     C***        ARBVAS    IFEQ *BLANKS
052300961115     C***                  MOVELWDIVIT    WVAL
052400961115     C***                  ELSE
052500961115     C***                  MOVELARBVAS    WVAL
052600961115     C***                  END
052700961115     C***                  MOVELWTD00     D00,X
052800961115     C***                  END
052900960530     C*  Se c'è C/Assegno se c'è
053000960530     C                   Z-ADD     1             Y                 3 0
053100960530     C     ARBCBO        LOOKUP    C3A(Y)                                 32
053200960530     C     *IN32         IFEQ      '1'
053300960530     C                   MOVEL     D3A(Y)        DS3A
053400960530     C     §3AFCA        IFNE      0
053500051114     C     KAR9          CHAIN     FIAR901L                           31
053600960530     C     AR9CAS        IFGT      0
053700960530     C                   ADD       1             X
053800960705     C                   MOVEL     '22 '         WTPI
053900960530     C                   Z-ADD     AR9CAS        WIMP
054000970221     C                   MOVE      '0'           WIMP
054100960530     C*  Se divisa a blanks imposto italiana
054200960530     C     AR9VCA        IFEQ      *BLANKS
054300960530     C                   MOVEL     WDIVIT        WVAL
054400960530     C                   ELSE
054500960530     C                   MOVEL     AR9VCA        WVAL
054600960530     C                   END
054700960530     C                   MOVEL     WTD00         D00(X)
054800960530     C                   END
054900960530     C*
055000960530     C                   END
055100960530     C                   END
055200960705     C*  Scrittura record D00
055300960705     C                   CLEAR                   WDAT
055400960705     C                   MOVEL     EDTD00        WDAT
055500960726     C                   Z-ADD     §MSNUM        SUMPRG
055600960705     C                   MOVEL     'TD00'        SUMTPR
055700960705     C                   MOVEL     WDAT          SUMDAT
055800960704     C                   WRITE     RRIFCSUM
055900960530     C*
056000960530     C                   ENDSR
056100960530     C*--------------------------------------------------------------*
056200960530     C* WRTD05: scirvo tipo record D05 in IFCSUM                    -*
056300960530     C*--------------------------------------------------------------*
056400960530     C     WRTD05        BEGSR
056500960530     C*
056600960530     C                   CLEAR                   EDTD05
056700960628     C                   MOVEL     'TD05'        SUMTPR
056800960530     C*  Controllo se ci sono delle note di consegna
056900960530     C*  Reperisco note spedizione
057000960530     C                   MOVEL     '8'           KTRC
057100960530     C                   Z-ADD     0             X
057200060213     C     KAR4          CHAIN     fiar401L                           32
057300960530     C     *IN32         IFEQ      '0'
057400960530     C                   ADD       1             X
057500960530     C                   MOVEL     AR4NOT        D05(X)
057600970203     C*  Se è una bolla di reso reperisco rif. originale patner
057700970203     C*  per scriverlo nelle note
057800970203     C     ARBFBR        IFEQ      'S'
057900970203     C                   EXSR      RIFPAT
058000970203     C                   END
058100970203     C                   END
058200960530     C                   MOVEL     '9'           KTRC
058300060213     C     KAR4          CHAIN     fiar401L                           32
058400960530     C     *IN32         IFEQ      '0'
058500960530     C                   ADD       1             X
058600960530     C                   MOVEL     AR4NOT        D05(X)
058700960530     C                   END
058800960628     C*  Se ho trovato qualche nota scrivo record D05
058900960628     C     X             IFGT      0
059000960716     C                   MOVEL     'SIN'         DB4451
059100960628     C                   END
059200130215      *
059300960628     C*  Se C'è il contrassegno scrivo mod.pagamento
059400960716     C     AR9CAS        IFNE      0
059500130215      *
059600960628     C     X             IFGT      0
059700960701     C                   MOVEL     'PAI'         DBE451
059800130215      *
059900130215      * la chiave deve essere di 35 non più di solo 2 caratteri
060000130215     C                   movel     ar9TIC        field35          35
060100130215      *
060200130215      * solo se presente il suffisso per un potenziale codice Fuori Standard
060300130215      *  da inviare al posto dello standard e se però non lo trova deve
060400130215      *   vedere con lo standard.
060500130215     c                   if        §pzSUFFtab <> *blank
060600130215     C                   move      §pzSUFFtab    field35
060700130215     c                   end
060800130215      *
060900130215      *  prova con chiave completa di 35 caratteri:
061000130215     C                   Z-ADD     1             Y
061100130215     C     field35       LOOKUP    TPIkey(Y)                              33
061200130215     C**** AR9TIC ****** LOOKUP    TPI(Y)                                 33
061300130215      * se non lo ha trovato prova con la chiave standard:
061400130215     C  n33              move      *blank        Due_blanks        2
061500130215     C  n33              move      Due_blanks    field35
061600130215     C  n33field35       LOOKUP    TPIkey(Y)                              33
061700130215      *
061800970204     C   33              MOVEL     *BLANKS       DBF440
061900970204     C   33              MOVEL     C1A(Y)        DBF440
062000130215      *
062100130215      *  Se ne avesse più di uno di tipi incasso
062200960628     C                   ELSE
062300130215      *  imposta nel secondo
062400960628     C                   MOVEL     'PAI'         DB4451
062500130215      *
062600130215      * la chiave deve essere di 35 non più di solo 2 caratteri
062700130215     C                   movel     ar9TIC        field35          35
062800130215      *
062900130215      * solo se presente il suffisso per un potenziale codice Fuori Standard
063000130215      *  da inviare al posto dello standard e se però non lo trova deve
063100130215      *   vedere con lo standard.
063200130215     c                   if        §pzSUFFtab <> *blank
063300130215     C                   move      §pzSUFFtab    field35
063400130215     c                   end
063500130215      *
063600130215      *  prova con chiave completa di 35 caratteri:
063700130215     C                   Z-ADD     1             Y
063800130215     C     field35       LOOKUP    TPIkey(Y)                              33
063900130215     C**** AR9TIC *****  LOOKUP    TPI(Y)                                 33
064000130215      * se non lo ha trovato prova con la chiave standard:
064100130215     C  n33              move      *blank        Due_blanks        2
064200130215     C  n33              move      Due_blanks    field35
064300130215     C  n33field35       LOOKUP    TPIkey(Y)                              33
064400130215      *
064500960716     C   33              MOVEL     *BLANKS       DB4440
064600960716     C   33              MOVEL     C1A(Y)        DB4440
064700130215      *
064800960628     C                   END
064900960628     C                   END
065000130215      *
065100960530     C*  Se ho trovato qualche nota scrivo record D05
065200960530     C     X             IFGT      0
065300960701     C     AR9CAS        ORGT      0
065400960705     C                   CLEAR                   WDAT
065500960705     C                   MOVEL     EDTD05        WDAT
065600960705     C                   MOVEL     'TD05'        SUMTPR
065700960726     C                   Z-ADD     §MSNUM        SUMPRG
065800960705     C                   MOVEL     WDAT          SUMDAT
065900960704     C                   WRITE     RRIFCSUM
066000960530     C                   END
066100960530     C*
066200960530     C                   ENDSR
066300170419     C*--------------------------------------------------------------*
066400170419     C* WRTD05_Alert: scrivo tipo record D05 x PREDICT ALERT mail/SMS*
066500170419     C*--------------------------------------------------------------*
066600170419     C     WRTD05_Alert  BEGSR
066700170419     C*
066800170419     C                   CLEAR                   EDTD05
066900170419     C                   MOVEL     'TD05'        SUMTPR
067000170419     C*
067100170419     C*  Se deve  avvisare il Destinatario
067200170419     C*     con MAIL
067300170419     C                   IF          Invia_alert_mail <> *blank
067400170419     C                   MOVEL     'EML'         DB4451
067500170419     c                   eval      DB4440 = Invia_alert_mail
067600170419     C*     con SMS
067700170419     C                   if          Invia_alert_SMS <> *blank
067800170419     C                   MOVEL     'SMS'         DBE451
067900170419     c                   eval      DBF440 = Invia_alert_SMS
068000170419     c                   end
068100170419      *
068200170419     C                   Else
068300170419      *
068400170419      *   altrimenti con solo SMS
068500170419     C                   MOVEL     'SMS'         DB4451
068600170419     c                   eval      DB4440 = Invia_alert_SMS
068700170419     C                   endIF
068800170419      *
068900170419     C                   CLEAR                   WDAT
069000170419     C                   MOVEL     EDTD05        WDAT
069100170419     C                   MOVEL     'TD05'        SUMTPR
069200170419     C                   Z-ADD     §MSNUM        SUMPRG
069300170419     C                   MOVEL     WDAT          SUMDAT
069400170419     C                   WRITE     RRIFCSUM
069500170419     C*
069600170419     C                   ENDSR
069700960530     C*--------------------------------------------------------------*
069800120119     C* WRTD10: scrivo tipo record D10 in IFCSUM                    -*
069900960530     C*--------------------------------------------------------------*
070000960530     C     WRTD10        BEGSR
070100960530     C*
070200960530     C                   CLEAR                   EDTD10
070300960530     C                   MOVEL     '5  '         DC4055
070400960626     C                   MOVEL     'EEL'         DC1131
070500980729      *  Imposto riferimento mittente alfabetico
070600960708     C                   Z-ADD     0             X
070700050518      ** su tab.PU
070800120119      **-------      se nell'AGE non si vuole avere la spedizione Bartolini
070900960905     C     D82NSA        IFEQ      'N'
071000980729      *
071100960905     C     ARBRMN        IFNE      0
071200960708     C                   ADD       1             X
071300960708     C                   MOVEL     'AGE'         WTPR
071400060505     C                   MOVEL(P)  ARBRMN        WRIF
071500960708     C                   MOVEL     WTD10         D10(X)
071600050518      *
071700960708     C                   ELSE
071800980729      *
071900960708     C     ARBRMA        IFNE      *BLANKS
072000960708     C                   ADD       1             X
072100960708     C                   MOVEL     'AGE'         WTPR
072200060505     C                   MOVEL(P)  ARBRMA        WRIF
072300960708     C                   MOVEL     WTD10         D10(X)
072400960708     C                   END
072500960906     C                   END
072600050518      **-------
072700960905     C                   ELSE
072800120119      *
072900120119      **-------  se nell'AGE si deve impostare la spedizione Bartolini
073000960905     C                   ADD       1             X
073100960905     C                   MOVEL     'AGE'         WTPR
073200120119      *
073300960905     C     D82AGE        IFLE      15
073400060505     C                   MOVEL(P)  SPE01         WRIF
073500960905     C                   ELSE
073600060505     C                   MOVEL(P)  DSSPE         WRIF
073700960905     C                   END
073800050518      **
073900960905     C                   MOVEL     WTD10         D10(X)
074000050518      **
074100050518      **  Se c'è anche un riferimento numerico del cliente
074200050518     C                   if        arbRMN <> 0
074300050518     C                   add       1             x
074400050518     C                   Movel     'CU '         wTPR
074500060110      *
074600060110      * ?------------------------------------------------------------------ */
074700060110      * ?Attenzione: per la trasmissione DPD occorre impostare il Parcel Nr.
074800060110      * ?  x SEUR quindi il tipo trasmissione x DPD è --> "S" deve prendere
074900060110      * ?  il Parcel dal FIAR4 con tipo record "F" i primi 12 caratteri
075000060110      * ?------------------------------------------------------------------ */
075100060110     c                   if        D82ftt ='S'
075200060110      *
075300060622     C******             MOVEL     'F'           KTRC
075400060622     C                   MOVEL     'I'           KTRC
075500060622      *
075600060213     C     kar4          CHAIN     fiar401L
075700060622      *
075800060213     C                   if        %Found(fiar401L)
075900060110     c                   clear                   fld15a
076000060622     c                   movel     AR4NOT        dsBl4i
076100060622     c                   movel     §B4IPN        fld15a
076200060622      *
076300060622     C******             eval      fld15a = %subst(ar4NOT:1:12)
076400060622     c******             else
076500060622      **** altrimenti prova con il riferimento numerico come fa normalmente.
076600060622     c******             move      arbRMN        fld15a
076700060622      *
076800060110     c                   end
076900060110      *
077000120119      * ?  x non DPD ossia in ogni altro caso
077100060110     c                   else
077200050518     c                   move      arbRMN        fld15a
077300060110     c                   end
077400060110      *
077500050518     c                   movea     fld15a        a15
077600050518     c                   do        15            ay                3 0
077700050518     c                   if        a15(ay) <> '0'
077800050518     c                   leave
077900050518     c                   End
078000050518     c                   Enddo
078100070212      * se trasm SEUR
078200070212     c                   if        D82ftt ='S'
078300070212     c                   eval      wRIF = %subst(fld15a: 1: 14)
078400070212     c                   else
078500070212      * se altro
078600050518     c                   eval      wRIF = %subst(fld15a: ay: (16-ay))
078700070212     c                   end
078800050518      **
078900050518     C                   Movel     wTD10         d10(x)
079000050518     C                   End
079100050518      **
079200960905     C                   END
079300120119      **------- -----------------------------------------------
079400120119      ** al POSTO del riferimento RFF+CU viene inviato il riferimento alternativo
079500120119      **   se è stato chiesto:
079600120119      **     per una bolla generata da un ORM, l'invio del riferimento originale
079700120119      **     alfanumerico dell'ORM
079800120119      **        oppure
079900120119      **     per una bolla generata da un RESO, l'invio del riferimento originale
080000120119      **     alfanumerico presente sulla bolla di IMPORT
080100120119      **
080200120119     c                   IF         Richiesta_sostituzione_CU ='S' and
080300120119     c                               (Bolla_di_RESO = 'S' or Bolla_da_ORM = 'S')
080400120119      **
080500120119      * se l'ultimo riferimento impostato NON era il CU occorre incrementare
080600120120     C                   if        wTPR <>'CU '
080700120119     C                   add       1             x
080800120119     c                   end
080900120120      *
081000120120      ** se è un Reso prevale sull'ORM
081100120119     c                   if        Bolla_di_RESO = 'S'
081200120119     C                   Z-ADD     bolAAO        KAAP
081300120119     C                   Z-ADD     bolLPO        KLPP
081400120119     C                   Z-ADD     bolNRO        KNRP
081500120119     C                   Z-ADD     bolNSO        KNPP
081600120119     C                   MOVEL     'E'           KTRC
081700120119     C     KBL4          CHAIN     Fiar401L
081800120119     c                   if           %Found(Fiar401L)
081900120120     c                   eval       dsbl4e = ar4not
082000120119     c                   eval         wRIF = §B4ERP
082100120120     C                   if         Qualificatore_RESO <> *blank
082200120119     C                   eval         wTPR = Qualificatore_RESO
082300120120     c                   end
082400120119      **
082500120119      **  se il riferimento deve essere numerico
082600120119     C                   if          Tipo_Rif_RESO ='N'
082700120119     c                   eval      num = %len(%Trim(wrif))
082800120119     c                   do        num
082900120119     c                   if        %subst(wrif:1:1)<>'0'
083000120119     c                   leave
083100120119     c                   else
083200120119     c                   eval      wrif = %subst(wrif:2:%len(wrif))
083300120119     c                   endif
083400120119     c                   enddo
083500120119     c                   end
083600120120      * solo se presente il riferimento
083700120120     c                   if        wrif <> *blank
083800120119     C                   movel     wTD10         d10(x)
083900120119     c                   end
084000120120     c                   end
084100120119      **
084200120119     c                   eLSe
084300120119      **
084400120119      ** se Non è un Reso ed è un ORM
084500120119     c                   if        Bolla_da_ORM = 'S'
084600120119     C                   Z-ADD     §B4POE        ORMPOE
084700120119     C                   Z-ADD     §B4NSR        ORMNSR
084800120119     C                   Z-ADD     §B4NOR        ORMNOR
084900120119     C                   Z-ADD     §B4NRV        ORMNRV
085000120119     C     Korm          CHAIN     Fnorm01L
085100120119     c                   if           %Found(Fnorm01L)
085200120120     C                   if         Qualificatore_ORM <> *blank
085300120119     C                   eval         wTPR = Qualificatore_ORM
085400120120     c                   end
085500120119     c                   eval         wRIF = ormRFA
085600120119      **
085700120119      **  se il riferimento deve essere numerico
085800120119     C                   if          Tipo_RIf_ORM ='N'
085900120119     c                   eval      num = %len(%Trim(wrif))
086000120119     c                   do        num
086100120119     c                   if        %subst(wrif:1:1)<>'0'
086200120119     c                   leave
086300120119     c                   else
086400120119     c                   eval      wrif = %subst(wrif:2:%len(wrif))
086500120119     c                   endif
086600120119     c                   enddo
086700120119     c                   end
086800120120      * solo se presente il riferimento
086900120120     c                   if        wrif <> *blank
087000120119     C                   movel     wTD10         d10(x)
087100120119     c                   end
087200120120     c                   end
087300120119      **
087400120119     c                   end
087500120119     c                   endIF
087600120119      **
087700120119     c                   end
087800120119      **------- -----------------------------------------------
087900000327      *  Scrivo segmento "FF" per Mittenti particolari
088000000327     C                   eval      yy = 1
088100000327     C     arbksc        lookup    CFF(YY)                                30
088200000327     C                   IF         *in30 = *on
088300000327     C                   add       1             X
088400000327     C                   movel     'FF '         WTPR
088500000327     C                   movel(P)  DFF(YY)       WRIF
088600000327     C                   movel     WTD10         D10(X)
088700000327     C                   ENDIF
088800980729     C*
088900960530     C*  Reperisco da EDTAB codice bolla corrispondente
089000960530     C                   Z-ADD     1             X
089100961125     C* Se franco frontiera imposto 'A' come se fosse un porto
089200961125     C* assegnato
089300960530     C                   MOVEL     §3ATB1        WPOR              1
089400961125     C     ARBFBR        IFEQ      'F'
089500961125     C                   MOVEL     'A'           WPOR
089600961125     C                   END
089700961204     C     §3ATB1        IFEQ      'FS'
089800961204     C                   MOVEL     'A'           WPOR
089900961204     C                   END
090000961204     C     §3ATB1        IFEQ      'AS'
090100961204     C                   MOVEL     'F'           WPOR
090200961204     C                   ENDIF
090300090223      ***
090400090223      ***  Attenzione:
090500090223      ***   se bolla in assegnato, controllare se già tassato e AR6KSC è
090600090223      ***  un cliente italia dire al partner che è franco
090700090223      ***  oppure
090800090223      ***  con il cliente preso dal FIAR4 con Tip.Rec."A"
090900090223      ***  se è un cliente italia dire al partner che è franco
091000090223      ***
091100090223     C                   if          WPOR = 'A'
091200090223     C     KAR6          chain     fiar601l
091300090223      * se trovato già fatturato come cliente Italia
091400090223     c                   if        %Found(fiar601l) and ar6KSC <= 2999999
091500090223     c                             and ar6KSC >0
091600090223     C                   eval        WPOR = 'F'
091700090223     C                   endIF
091800090223      *
091900090223      * altrumenti se proposto come cliente Italia
092000090223     c                   if         WPOR = 'A' or not %Found(fiar601l)
092100090223     C                   MOVEL     'A'           KTRC
092200090223     C     KAR4          CHAIN     fiar401L                           32
092300090223     c                   if        %Found(fiar401L)
092400090223     c                   eval      dsbl4a = ar4NOT
092500090223      * se Italia
092600090223     c                   if        §B4AKS <= 2999999 and §B4AKS >0
092700090223     C                   eval        WPOR = 'F'
092800090223     C                   endIF
092900090223     C                   endIF
093000090223     C                   endIF
093100090223     C                   endIF
093200090223      ***
093300140428      ***  se il partner ha un suffisso particolare, occorre verificare
093400140428      ***  se vi sono codici da trascodificare per lui prima di vedere lo standard
093500140428      ***  cosa prevede.
093600140428     c                   clear                   porto_suff        3
093700140428     c                   movel     Wpor          porto_suff
093800140428     c                   move      §pzSUFFtab    porto_suff
093900140428     C     porto_suff    LOOKUP    CTBkey(X)                              32
094000140428      *
094100140428      ***  se NON trova cerca come standard
094200140428     C*****WPOR          LOOKUP    CTB(X)                                 32
094300140428     c  n32              clear                   porto_suff
094400140428     c  n32              movel     Wpor          porto_suff
094500140428     C  n32porto_suff    LOOKUP    CTBkey(X)                              32
094600140428      *
094700960530     C     *IN32         IFEQ      '1'
094800960530     C                   MOVEL     TCO(X)        DC4053
094900960530     C                   END
095000960530     C* Dati traporto
095100060118     C******             MOVEL     '20 '         DC8051
095200060118     C******             MOVEL     '3  '         DC8067
095300060118      * ?***  Tolto xchè basta una sola volta in testata.
095400060118      * ?***  Dopo attivazione CEDINTESA il nuovo traduttore,x ogni dettaglio,
095500060118      * ?***  riportava il TDT+20+3 cosa che stranamente non faceva il vecchio
095600060118      * ?***  traduttore AS400. (segnalato da Lynx il problema)
095700060118     C******
095800960530     C*  Scrittura record D10
095900960705     C                   CLEAR                   WDAT
096000960705     C                   MOVEL     EDTD10        WDAT
096100960726     C                   Z-ADD     §MSNUM        SUMPRG
096200960705     C                   MOVEL     'TD10'        SUMTPR
096300960705     C                   MOVEL     WDAT          SUMDAT
096400960704     C                   WRITE     RRIFCSUM
096500960530     C*
096600960530     C                   ENDSR
096700960530     C*--------------------------------------------------------------*
096800960530     C* WRTD15: scirvo tipo record D15 in IFCSUM                    -*
096900960530     C*--------------------------------------------------------------*
097000960530     C     WRTD15        BEGSR
097100960530     C*
097200031211     C* Prima deve essere scritto il Mittente e poi il destinatario
097300031211     C* questa è una regola altrimenti non funzionano le informazioni
097400031211     C* di riferimento del destinatario/telefono
097500031211     C*
097600031211     C*  Scrivo D15 x mittente
097700031211     C                   CLEAR                   EDTD15
097800031211     C                   MOVEL     'CZ'          DD3035
097900031211     C* ---------
098000031211     C*  Se codice cliente mittente = 0 metto codice cliente spedizione
098100031211     C*   (prima metteva il cod. Part.IVA Europeo)
098200031211     C                   clear                   WIVAM            16
098300031211     C                   MOVEL     ARBCCM        WIVAM            16
098400031211     C     ARBCCM        IFEQ      0
098500031211     C                   MOVEL     ARBKSC        WIVAM
098600031211     C                   END
098700031211     C* ---------
098800031211     C                   MOVEL     WIVAM         DD3039
098900031211     C*  Altri dati destinatario
099000031211     C                   MOVEL     ARBRSM        DD3036
099100031211     C                   MOVEL     ARBINM        DD3042
099200031211     C                   MOVEL     ARBLOM        DD3164
099300031211     C                   MOVEL     ARBCAM        DD3251
099400031211     C                   Z-ADD     1             YY
099500031211     C     ARBNZM        LOOKUP    C15(YY)                                32
099600031211     C   32ISO(YY)       COMP      *BLANKS                            3232
099700031211     C  N32              MOVEL     'IT'          DD3207
099800031211     C   32              MOVEL     ISO(YY)       DD3207
099900031211     C*  Scrittura record D15
100000031211     C                   CLEAR                   WDAT
100100031211     C                   MOVEL     EDTD15        WDAT
100200031211     C                   Z-ADD     §MSNUM        SUMPRG
100300031211     C                   MOVEL     'TD15'        SUMTPR
100400031211     C                   MOVEL     WDAT          SUMDAT
100500031211     C                   WRITE     RRIFCSUM
100600031211     C*
100700960530     C                   CLEAR                   EDTD15
100800960530     C*  Scrivo D15 x destinatario
100900960530     C                   MOVEL     'CN'          DD3035
101000960712     C*  Reperisco P.Iva destinatario
101100960712     C                   MOVEL     'P'           KTRC
101200060213     C     KAR4          CHAIN     fiar401L                           32
101300960712     C     *IN32         IFEQ      '0'
101400961230     C                   MOVE      AR4NOT        WIVAD            16
101500961230     C                   MOVEL     WIVAD         DD3039
101600011031     C                   MOVEL     AR4NOT        WIVAM            16
101700960712     C                   ELSE
101800960730     C                   MOVEL     ARBKSC        DD3039
101900960712     C*  Se codice cliente mittente = 0 metto codice
102000960712     C*  destinatario
102100960712     C                   MOVEL     ARBCCM        WIVAM            16
102200960712     C     ARBCCM        IFEQ      0
102300960730     C                   MOVEL     ARBKSC        WIVAM
102400960712     C                   END
102500960712     C                   END
102600960530     C*  Altri dati destinatario
102700960530     C                   MOVEL     ARBRSD        DD3036
102800960530     C                   MOVEL     ARBIND        DD3042
102900960530     C                   MOVEL     ARBLOD        DD3164
103000080924      * chiodo sul CAP
103100080924      * x l'IRLANDA "EIRE" nel CAP deve essere sostituito con "0000"
103200080924     c                   if        arbcad = 'EIRE '
103300080924     C                   MOVEL     '0000'        DD3251
103400080924     c                   else
103500960530     C                   MOVEL     ARBCAD        DD3251
103600080924     c                   end
103700960822     C                   Z-ADD     1             YY                3 0
103800960822     C     ARBNZD        LOOKUP    C15(YY)                                32
103900960822     C     *IN32         IFEQ      '0'
104000960822     C     D82NAZ        LOOKUP    C15(YY)                                32
104100960822     C                   END
104200960823     C*
104300960823     C   32ISO(YY)       COMP      *BLANKS                            3232
104400960822     C  N32              MOVEL     D82NAZ        DD3207
104500960822     C   32              MOVEL     ISO(YY)       DD3207
104600960530     C*  Reperisco seconda ragione sociale destinatario
104700960530     C                   MOVEL     'D'           KTRC
104800060213     C     KAR4          CHAIN     fiar401L                           32
104900960530     C     *IN32         IFEQ      '0'
105000960530     C                   MOVEL     AR4NOT        DDA036
105100960530     C                   END
105200031210      *
105300031210      *  se presenti riporta i riferimenti referente x la consegna e
105400031210      *   il riferimento telefonico
105500031210     c                   movel     'GEN'         ktrd
105600031210     c                   clear                   DAr5Gen
105700031210     c     kar5          chain     fiar501L
105800031210     c                   if        %Found(fiar501L)
105900031210     c                   Movel     Ar5Uni        DAr5Gen
106000031212      * devono comunque essere scritti insieme: CTA..... COM altrimenti non funziona
106100031212     c                   if        §ar5ref <> *blank or
106200031212     c                             §ar5teld <> *blank
106300031212     C                   MOVEL     'IC'          DD3139
106400031212     C                   MOVEL     'TE'          DD3155
106500031210     c                   if        §ar5ref <> *blank
106600031210     c                   movel     §ar5ref       DD3412
106700031212     c                   else
106800031212     c                   movel(P)  'X'           DD3412
106900031212     c                   end
107000031210     c                   if        §ar5teld <> *blank
107100031210     c                   movel     §ar5teld      DD3148
107200031212     c                   else
107300031212     c                   movel(P)  'X'           DD3148
107400031210     c                   end
107500031210     c                   end
107600031212     c                   end
107700031210      *
107800960530     C*  Scrittura record D15
107900960705     C                   CLEAR                   WDAT
108000960705     C                   MOVEL     EDTD15        WDAT
108100960726     C                   Z-ADD     §MSNUM        SUMPRG
108200960705     C                   MOVEL     'TD15'        SUMTPR
108300960705     C                   MOVEL     WDAT          SUMDAT
108400960704     C                   WRITE     RRIFCSUM
108500960530     C*
108600960530     C                   ENDSR
108700960530     C*--------------------------------------------------------------*
108800960530     C* WRTD20: scirvo tipo record D20 in IFCSUM                    -*
108900960530     C*--------------------------------------------------------------*
109000960530     C     WRTD20        BEGSR
109100960530     C*
109200960530     C                   CLEAR                   EDTD20
109300960530     C*  Scrivo totale colli spedizione
109400960530     C                   Z-ADD     99999         DE1496
109500960530     C                   Z-ADD     ARBNCL        DE7224
109600960807     C                   MOVEL     'CT'          DE7065
109700960530     C*  Addiziono in totale colli
109800960530     C                   ADD       ARBNCL        WTONCL
109900100408     C*  Natura Merce
110000100408     C                   IF        §PUFTXAAA = 'S' and arbNAS <> *blank
110100100408     C                   MOVEL     'AAA'         DE4451
110200100408     C                   MOVEL     arbNAS        DE4440
110300100408     c                   end
110400100408     C*
110500960530     C*  Scrittura record D20
110600960705     C                   CLEAR                   WDAT
110700960705     C                   MOVEL     EDTD20        WDAT
110800960726     C                   Z-ADD     §MSNUM        SUMPRG
110900960705     C                   MOVEL     'TD20'        SUMTPR
111000960705     C                   MOVEL     WDAT          SUMDAT
111100960704     C                   WRITE     RRIFCSUM
111200960530     C*
111300960530     C                   ENDSR
111400960530     C*--------------------------------------------------------------*
111500960530     C* WRTD25: scirvo tipo record D25 in IFCSUM                    -*
111600960530     C*--------------------------------------------------------------*
111700960530     C     WRTD25        BEGSR
111800960530     C*
111900960530     C                   CLEAR                   EDTD25
112000960530     C*  Specifico peso
112100960530     C                   MOVEL     'WT '         DF6311
112200960903     C                   MOVEL     'AAG'         DF6313
112300960530     C                   MOVEL     'KGM'         DF6411
112400150825     C*
112500150825     C*  reperisce il volume/peso da CML o da Bollettazione
112600150825     c                   exsr      VOL_PES
112700150825      ** -------
112800150825      **  SOSTITUITO tutto dal calcolo con la VOL_PES
112900961126     C* Determino peso tassabile per le spedizioni in assegnato
113000961126     C* (è il maggiore fra ARBPKF e il risultato delle moltiplicazione)
113100150825     C***  WPOR          IFEQ      'A'
113200150825     C***  §7AKPM        MULT(H)   ARBVLF        PESPRT            7 1
113300150825     C***  ARBPKF        IFGT      PESPRT
113400150825     C***                Z-ADD     ARBPKF        PESPRT
113500150825     C***                END
113600150825     C***                ELSE
113700961126     C* Per le spedizioni in franco,invece,stampo il peso da fatturare
113800150825     C***                Z-ADD     ARBPKF        PESPRT
113900150825     C***                END
114000150825     C***
114100150825     C***                Z-ADD(H)  PESPRT        WPKF              7 0
114200150825      *
114300150825      **  toglie il decimale
114400150825     C                   Z-ADD(H)  wpes          wpes0             7 0
114500150825     C     wpes0         IFEQ      0
114600150825     C                   Z-ADD     1             wpes0
114700150825     C                   END
114800150825     C                   Z-ADD     wpes0         DF6314
114900150825      *
115000150825     C** Addiziono in totale peso x intero messaggio
115100150825     C                   ADD       wpes0         WTOPKF
115200150825      ** -------
115300960530     C*  Scrittura record D25
115400960705     C                   CLEAR                   WDAT
115500960705     C                   MOVEL     EDTD25        WDAT
115600960726     C                   Z-ADD     §MSNUM        SUMPRG
115700960705     C                   MOVEL     'TD25'        SUMTPR
115800960705     C                   MOVEL     WDAT          SUMDAT
115900960704     C                   WRITE     RRIFCSUM
116000010424     C*
116100960530     C*  Specifico volume
116200010730     C     §PUVOL        IFeq      'S'
116300010424     C*
116400010424     C                   CLEAR                   EDTD25
116500010424     C                   MOVEL     'VOL'         DF6311
116600010424     C                   MOVEL     'MTQ'         DF6411
116700150825      ** -------
116800150825     C***                Z-ADD(H)  ARBVLF        WVLF              4 2
116900150825      *
117000150825     C                   Z-ADD(H)  wvol          wvol0             4 2
117100150825     C     wvol0         IFEQ      *ZEROS
117200150825     C                   Z-ADD     0,01          wvol0
117300150825     C                   END
117400150825     C                   Z-ADD     wvol0         DF6314
117500150825      *
117600150825     C** Addiziono in totale peso x intero messaggio
117700150825     C                   ADD       wvol0         WTOVLF
117800150825      ** -------
117900960530     C*  Scrittura record D25
118000010424     C                   CLEAR                   WDAT
118100010424     C                   MOVEL     EDTD25        WDAT
118200010424     C                   Z-ADD     §MSNUM        SUMPRG
118300010424     C                   MOVEL     'TD25'        SUMTPR
118400010424     C                   MOVEL     WDAT          SUMDAT
118500010424     C                   WRITE     RRIFCSUM
118600010424     C*
118700010424     C                   End
118800960530     C*
118900960530     C                   ENDSR
119000150825     C*****************************************************************
119100150825     C*  Reperisce Peso/Volume o da CML o da Bollettato
119200150825     C*****************************************************************
119300150825     C     VOL_PES       BEGSR
119400150825     C*
119500150825     c     *like         define    arbvlb        wvol
119600150825     c     *like         define    arbpkb        wpes
119700150825     C     *LIKE         DEFINE    §qttpc        WTARA
119800150825      *
119900150825      * Prima cerca il volume fra bollettato e CML
120000150825     C     arbncR        ifeq      arbncl
120100150825      * se è considerata un'unica spedizione
120200150825     C                   z-add     ARBVLC        WVOL
120300150825     C                   else
120400150825     C*
120500150825     C     ARBVLC        ifge      ARBVLb
120600150825     C                   z-add     ARBVLC        WVOL
120700150825     C                   else
120800150825     C                   z-add     ARBVLb        WVOL
120900150825     C                   end
121000150825     C*
121100150825     C                   endif
121200150825      *
121300150825     C* PESO VDL - TARA
121400150825     C     §QTTPC        MULT      arbncp        WTARA
121500150825     C                   Z-ADD     arbPKC        wpes
121600150825     c                   sub(h)    wtara         wpes
121700150825      *
121800150825      * Poi cerca il peso fra bollettato e CML
121900150825     C     arbncP        ifeq      arbncl
122000150825      * se è considerata un'unica spedizione
122100150825     C                   ELSE
122200150825     C*  altrimenti se il bollettato è superiore passo il Bollettato.
122300150825     C     wpes          iflt      arbPKb
122400150825     C                   Z-ADD     arbPKb        wpes
122500150825     C                   end
122600150825     C*
122700150825     C                   ENDIF
122800150825     C*
122900150825     C                   ENDSR
123000960530     C*--------------------------------------------------------------*
123100960530     C* WRTD30: scirvo tipo record D30 in IFCSUM                    -*
123200960530     C*--------------------------------------------------------------*
123300960530     C     WRTD30        BEGSR
123400960530     C*
123500960530     C*  Scrivo dettaglio segnacolli
123600960530     C     KART          CHAIN     FNART01L                           31
123700120330      *
123800960530     C     *IN31         DOWEQ     '0'
123900960530     C                   CLEAR                   EDTD30
124000120330      *
124100120330     C                   Z-ADD     1             X
124200120330      *
124300960530     C*  Scrivo fino a 10 segnacolli o fino a che non è finito dettag.
124400120330     C*****X             DOWLE     20
124500120330     C     X             DOWLE     Quanti_X_PCI
124600960530     C     *IN31         ANDEQ     '0'
124700120330      *
124800120413     C********           ADD       1             X
124900120330      *
125000051017     C*  Se tipo segnacollo 'E'=Euroexpress aggancio FIARS
125100980219     C     D82TSC        IFEQ      'E'
125200980219     C                   MOVEL     'E'           KTRC
125300980219     C                   Z-ADD     ARTFLS        KFLS
125400980219     C                   Z-ADD     ARTLNA        KLNA
125500980219     C                   Z-ADD     ARTNRS        KNRS
125600980219     C                   Z-ADD     ARTNSC        KNSC
125700051017     C     KARS          CHAIN     FIARS01L                           35
125800980219     C     *IN35         IFEQ      '0'
125900980219     C                   MOVEL     ARSNOT        D30(X)
126000980219     C                   END
126100980219     C                   ELSE
126200980219     C                   MOVEL     DSSGN         D30(X)
126300980219     C                   END
126400120413      *
126500120413      * spostato il contatore
126600120413     C                   ADD       1             X
126700960530     C     KART          READE     FNART01L                               31
126800960530     C                   END
126900120330      *
127000120330      *  Se presente un Qualificatore lo imposta
127100120330     c                   if          Qualificatore_PCI <> *blank
127200120330     c                   eval      DG4233 = Qualificatore_PCI
127300120330     c                   end
127400120330      *
127500960530     C*  Scivo record TD30
127600960705     C                   CLEAR                   WDAT
127700960705     C                   MOVEL     EDTD30        WDAT
127800980219     C                   Z-ADD     §MSNUM        SUMPRG
127900960705     C                   MOVEL     'TD30'        SUMTPR
128000960705     C                   MOVEL     WDAT          SUMDAT
128100960704     C                   WRITE     RRIFCSUM
128200120330     C*
128300960530     C                   END
128400960530     C*
128500960530     C                   ENDSR
128600970203     C*--------------------------------------------------------------*
128700970203     C* RIFPAT: Reperisco riferimento originale partner             -*
128800970203     C*--------------------------------------------------------------*
128900970203     C     RIFPAT        BEGSR
129000970203     C*
129100970203     C                   MOVEL     AR4NOT        TESTO1            5
129200020711     c                   clear                   lnpnot            4
129300020711     C                   eval      LNPnot = %subst(ar4not:21:4)
129400970203     C                   SETON                                        34
129500970203     C*  Se la nota partenza mi indica che il reso è stato effettuato
129600970203     C*  da giacenza mi posiziono si giac. e reperisco nr.spedizione
129700020711     C                   If        TESTO1 = COST2 and LNPnot='LNP.'
129800970203     C                   MOVEL     AR4NOT        RIFGIA
129900970203     C                   Z-ADD     GCAAGC        KAGC
130000970203     C                   Z-ADD     GCAFGC        KFGC
130100970203     C                   Z-ADD     GCANGC        KNGC
130200970203     C                   Z-ADD     0             KFRG
130300970203     C     KAGC          IFGT      60
130400970203     C                   ADD       1900          KAGC
130500970203     C                   ELSE
130600970203     C                   ADD       2000          KAGC
130700970203     C                   END
130800050309     C     KGCP          CHAIN     TIGCP02L                           31
130900970203     C                   Z-ADD     GCPAAS        KAAP
131000970203     C                   Z-ADD     GCPLNP        KLPP
131100970203     C                   Z-ADD     GCPNRS        KNRP
131200970203     C                   Z-ADD     GCPNSP        KNPP
131300060213     C* Mi posiziono su ar4 x aggangiare rif.partner
131400970203     C                   MOVEL     'E'           KTRC
131500060213     C     KBL4          CHAIN     Fiar401L                           34
131600970203     C                   ELSE
131700970203     C                   MOVEL     AR4NOT        TESTO3
131800001222     C*
131900970203     C*  altrimenti verifico se la nota è di reso fuori da giacenza
132000970203     C*  reperisco nr.spedizione da essa
132100970203     C     TESTO3        IFEQ      COST1
132200001222      *
132300001222     C                   MOVEL     'E'           KTRC
132400001222     C                   MOVEL     AR4NOT        dgtSPE
132500001222      *
132600001222     C*   controlla che ci siano solo numerici nel campo di testo
132700001222      *    altrimenti è come se non fosse riuscita la CHAIN seguente.
132800001222     C     digits        check     chkspe:1
132900001222      *
133000001222     C                   IF        %Found
133100001222     C                   seton                                        34
133200001222     C                   ELSE
133300001222     C*
133400001222     C                   MOVEL     AR4NOT        RIFSPE
133500970203     C                   Z-ADD     RAAS          KAAP
133600970203     C                   Z-ADD     RLNP          KLPP
133700970203     C                   Z-ADD     RNRS          KNRP
133800970203     C                   Z-ADD     RNSP          KNPP
133900970203     C     KAAP          IFGT      60
134000970203     C                   ADD       1900          KAAP
134100970203     C                   ELSE
134200970203     C                   ADD       2000          KAAP
134300970203     C                   END
134400001222     C*
134500060213     C* Mi posiziono su ar4 x aggangiare rif.partner
134600060213     C     KBL4          CHAIN     Fiar401L                           34
134700001222     C                   end
134800001222     C*
134900060213     C*  Se non trovato mi posiziono su ar4 con la  sped.originale
135000970203     C     *IN34         IFEQ      '1'
135100970203     C     KLBL          CHAIN     FNLBL02L                           31
135200970203     C     *IN31         IFEQ      '0'
135300970203     C                   Z-ADD     LBLAAO        KAAP
135400970203     C                   Z-ADD     LBLLPO        KLPP
135500970203     C                   Z-ADD     LBLNRO        KNRP
135600970203     C                   Z-ADD     LBLNSO        KNPP
135700970203     C                   MOVEL     'E'           KTRC
135800060213     C     KBL4          CHAIN     Fiar401L                           34
135900970203     C                   END                                                    trv.lbl
136000970203     C                   END                                                    trv bl4
136100970203     C*
136200970203     C                   END                                                    = testo3
136300970203     C                   END                                                    = testo1
136400970203     C*  Se trovato rif.partner originale lo scrivo nelle note
136500970203     C     *IN34         IFEQ      '0'
136600060213     C                   MOVEL     ar4NOT        WCOM15           15
136700970203     C                   MOVEL     D05(X)        WCOM50           50
136800970203     C                   MOVE      WCOM15        WCOM50
136900970203     C                   MOVEL     WCOM50        D05(X)
137000970203     C                   END
137100970203     C*
137200970203     C                   ENDSR
137300950428     C*****************************************************************
137400950428     C* OPERAZIONI INIZIALI
137500950428     C*****************************************************************
137600950428     C     *INZSR        BEGSR
137700010426     C*
137800960530     C* Definisco chiavi di accesso
137900950428     C     KARB          KLIST
138000960530     C                   KFLD                    KAAS
138100960530     C                   KFLD                    KLNP
138200960530     C                   KFLD                    KNRS
138300960530     C                   KFLD                    KNSP
138400960530     C     KART          KLIST
138500960530     C                   KFLD                    KAAS
138600960530     C                   KFLD                    KLNP
138700960530     C                   KFLD                    KNRS
138800960530     C                   KFLD                    KNSP
138900980219     C     KARS          KLIST
139000980219     C                   KFLD                    KFLS
139100980219     C                   KFLD                    KLNA
139200980219     C                   KFLD                    KNRS
139300980219     C                   KFLD                    KNSC
139400980219     C                   KFLD                    KTRC
139500970203     C     KLBL          KLIST
139600970203     C                   KFLD                    KAAS
139700970203     C                   KFLD                    KLNP
139800970203     C                   KFLD                    KNRS
139900970203     C                   KFLD                    KNSP
140000970203     C     KGCP          KLIST
140100970203     C                   KFLD                    KAGC
140200970203     C                   KFLD                    KFGC
140300970203     C                   KFLD                    KNGC
140400970203     C                   KFLD                    KFRG
140500970203     C     KBL4          KLIST
140600970203     C                   KFLD                    KAAP
140700970203     C                   KFLD                    KLPP
140800970203     C                   KFLD                    KNRP
140900970203     C                   KFLD                    KNPP
141000970203     C                   KFLD                    KTRC
141100950428     C     KAR4          KLIST
141200960530     C                   KFLD                    KAAS
141300960530     C                   KFLD                    KLNP
141400960530     C                   KFLD                    KNRS
141500960530     C                   KFLD                    KNSP
141600960530     C                   KFLD                    KTRC
141700031210     C     KAR5          KLIST
141800031210     C                   KFLD                    KAAS
141900031210     C                   KFLD                    KLNP
142000031210     C                   KFLD                    KNRS
142100031210     C                   KFLD                    KNSP
142200031210     C                   KFLD                    KTRD              3
142300090223     C     KAR6          KLIST
142400090223     C                   KFLD                    KAAS
142500090223     C                   KFLD                    KLNP
142600090223     C                   KFLD                    KNRS
142700090223     C                   KFLD                    KNSP
142800960530     C     KAR9          KLIST
142900960530     C                   KFLD                    KAAS
143000960530     C                   KFLD                    KLNP
143100960530     C                   KFLD                    KNRS
143200960530     C                   KFLD                    KNSP
143300960530     C     KTAB          KLIST
143400960530     C                   KFLD                    KKUT
143500960530     C                   KFLD                    KCOD
143600961126     C     KTAB1         KLIST
143700961126     C                   KFLD                    KKUT
143800961126     C                   KFLD                    KCOD
143900961126     C                   KFLD                    KKEY
144000960726     C     KTABE         KLIST
144100960726     C                   KFLD                    KCOD1
144200960726     C                   KFLD                    KKEY1
144300120119     C     Korm          KLIST
144400120119     C                   KFLD                    ORMPOE
144500120119     C                   KFLD                    ORMNSR
144600120119     C                   KFLD                    ORMNOR
144700120119     C                   KFLD                    ORMNRV
144800960530     C*  Definisco variabili
144900960726     C     *LIKE         DEFINE    TABKEY        KKEY1
145000960530     C     *LIKE         DEFINE    ARBAAS        KAAS
145100960530     C     *LIKE         DEFINE    ARBLNP        KLNP
145200960530     C     *LIKE         DEFINE    ARBNRS        KNRS
145300960530     C     *LIKE         DEFINE    ARBNSP        KNSP
145400960530     C     *LIKE         DEFINE    AR4TRC        KTRC
145500960530     C     *LIKE         DEFINE    TABCOD        KCOD1
145600960530     C     *LIKE         DEFINE    TBLKUT        KKUT
145700960530     C     *LIKE         DEFINE    TBLCOD        KCOD
145800961126     C     *LIKE         DEFINE    TBLKEY        KKEY
145900970203     C     *LIKE         DEFINE    GCPAGC        KAGC
146000970203     C     *LIKE         DEFINE    GCPFGC        KFGC
146100970203     C     *LIKE         DEFINE    GCPNGC        KNGC
146200970203     C     *LIKE         DEFINE    GCPFRG        KFRG
146300060213     C     *LIKE         DEFINE    ar4AAS        KAAP
146400060213     C     *LIKE         DEFINE    ar4LNP        KLPP
146500060213     C     *LIKE         DEFINE    ar4NRS        KNRP
146600060213     C     *LIKE         DEFINE    ar4NSP        KNPP
146700980219     C     *LIKE         DEFINE    ARSFLS        KFLS
146800980219     C     *LIKE         DEFINE    ARSLNA        KLNA
146900980219     C     *LIKE         DEFINE    ARSNSC        KNSC
147000970203     C* InizializzoIvariabil FtotaleOD    KCOD1
147100960530     C                   Z-ADD     0             WPROG             5 0
147200960530     C                   Z-ADD     0             WTONCL            9 0
147300961016     C                   Z-ADD     0             WTOPKF           10 1
147400961016     C                   Z-ADD     0             WTOVLF           12 3
147500960530     C* Imposto data del giorno
147600960530     C                   TIME                    W0140            14 0
147700960530     C                   MOVE      W0140         G02DAT            8 0
147800960530     C                   MOVEL     *BLANK        G02ERR
147900960530     C                   CALL      'XSRDA8'
148000960530     C                   PARM                    WLBDA8
148100960530     C                   MOVEL     G02INV        WOGGI             8 0
148200010424      *------------
148300010424     C* Caricamento Tabella Partner esteri
148400010424     C                   Z-ADD     0             X                 3 0
148500010424     C                   MOVEL     'PT'          TABCOD
148600010424     C     TABCOD        CHAIN     EDTAB01L                           30
148700010424     C     *IN30         DOWEQ     '0'
148800010424     C     TABFLG        IFEQ      *BLANKS
148900010424     C                   ADD       1             X
149000010424     C                   MOVEL     TABKEY        CPT(X)
149100010424     C                   MOVEL     TABUNI        DPT(X)
149200010424     C                   MOVEL     TABUNI        KSC(X)
149300010424     C                   END
149400010424     C     TABCOD        READE     EDTAB01L                               30
149500010424     C                   END
149600121105      *
149700121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
149800121105     c                   exsr      ChecK_PT
149900010730      *------------
150000010730     C* Caricamento Tabella Partner esteri aggiuntiva
150100010730     C                   MOVEL     'PU'          TABCOD
150200010730     C     TABCOD        CHAIN     EDTAB01L                           30
150300010730     C     *IN30         DOWEQ     '0'
150400010730     C     TABFLG        IFEQ      *BLANKS
150500010730     C                   MOVEL     TABKEY        CODPU            35
150600010730     C                   Z-ADD     1             X
150700010730     C     CODPU         LOOKUP    CPT(X)                                 32
150800010730     C   32              MOVEL     TABUNI        DPU(X)
150900010730     C                   END
151000010730     C     TABCOD        READE     EDTAB01L                               30
151100010730     C                   END
151200120119      *------------
151300120119     C* Caricamento Tabella Partner esteri aggiuntiva PV
151400120119     C                   MOVEL     'PV'          TABCOD
151500120119     C     TABCOD        CHAIN     EDTAB01L                           30
151600120119     C     *IN30         DOWEQ     '0'
151700120119     C     TABFLG        IFEQ      *BLANKS
151800120119     C                   MOVEL     TABKEY        CODPV            35
151900120119     C                   Z-ADD     1             X
152000120119     C     CODPV         LOOKUP    CPT(X)                                 32
152100120119     C   32              MOVEL     TABUNI        DPV(X)
152200120119     C                   END
152300120119     C     TABCOD        READE     EDTAB01L                               30
152400120119     C                   END
152500120705      *------------
152600120705     C* Caricamento Tabella Partner esteri aggiuntiva PZ
152700120705     C                   MOVEL     'PZ'          TABCOD
152800120705     C     TABCOD        CHAIN     EDTAB01L                           30
152900120705     C     *IN30         DOWEQ     '0'
153000120705     C     TABFLG        IFEQ      *BLANKS
153100120705     C                   MOVEL     TABKEY        CODPZ            35
153200120705     C                   Z-ADD     1             X
153300120705     C     CODPZ         LOOKUP    CPT(X)                                 32
153400120705     C   32              MOVEL     TABUNI        DPZ(X)
153500120705     C                   END
153600120705     C     TABCOD        READE     EDTAB01L                               30
153700120705     C                   END
153800170412      *------------
153900170412     C* Caricamento Tabella Partner esteri aggiuntiva PS (invio Alert)
154000170412     C                   MOVEL     'PS'          TABCOD
154100170412     C     TABCOD        CHAIN     EDTAB01L                           30
154200170412     C     *IN30         DOWEQ     '0'
154300170412     C     TABFLG        IFEQ      *BLANKS
154400170412     C                   MOVEL     TABKEY        CODPS            35
154500170412     C                   Z-ADD     1             X
154600170412     C     CODPS         LOOKUP    CPT(X)                                 32
154700170412     C   32              MOVEL     TABUNI        DPs(X)
154800170412     C                   END
154900170412     C     TABCOD        READE     EDTAB01L                               30
155000170412     C                   END
155100010424      *------------
155200960530     C* Caricamento Tabella Priorità trasporto
155300140428      *   LA TABELLA è COMUNQUE IGNORATA poichè utilizzata solo in IMPORT
155400140428      *   qui è stato cambiato il modo di inviare l'informazione del Tipo Servizio.
155500140428      *   Viene mandato solo in particolare situazione x indicare l'appuntamento oppure
155600140428      *   il priority o altro.
155700960530     C                   Z-ADD     0             X                 3 0
155800960530     C                   MOVEL     'TS'          KCOD1
155900960530     C     KCOD1         CHAIN     EDTAB01L                           30
156000960530     C     *IN30         DOWEQ     '0'
156100960530     C     TABFLG        IFEQ      *BLANKS
156200070220     c                   move      tabkey        due_alfa          2
156300070220     c                   if        due_alfa = *blank
156400960530     C                   ADD       1             X
156500960530     C                   MOVEL     TABKEY        PTR(X)
156600960530     C                   MOVEL     TABUNI        TSP(X)
156700070220     c                   end
156800960530     C                   END
156900960530     C     KCOD1         READE     EDTAB01L                               30
157000960530     C                   END
157100960716     C* Caricamento Tabella Tipo Incasso C/Assegno
157200960716     C                   Z-ADD     0             X                 3 0
157300960716     C                   MOVEL     '1A'          TABCOD
157400960716     C     TABCOD        CHAIN     EDTAB01L                           30
157500960716     C     *IN30         DOWEQ     '0'
157600960716     C     TABFLG        IFEQ      *BLANKS
157700960716     C                   ADD       1             X
157800960716     C                   MOVEL     TABKEY        TPI(X)
157900130215     C                   MOVEL     TABKEY        TPIkey(X)
158000960716     C                   MOVEL     TABUNI        C1A(X)
158100960716     C                   END
158200960716     C     TABCOD        READE     EDTAB01L                               30
158300960716     C                   END
158400960530     C* Caricamento Tabella termini consegna
158500960530     C                   Z-ADD     0             X                 3 0
158600960530     C                   MOVEL     'TB'          KCOD1
158700960530     C     KCOD1         CHAIN     EDTAB01L                           30
158800960530     C     *IN30         DOWEQ     '0'
158900960530     C     TABFLG        IFEQ      *BLANKS
159000070220     c                   move      tabkey        due_alfa          2
159100140428      ** carica tutto anche i fuori STD x particolarità legate al PTN da inviare
159200140428     c***********        if        due_alfa = *blank
159300960530     C                   ADD       1             X
159400960530     C                   MOVEL     TABKEY        TCO(X)
159500960530     C                   MOVEL     TABUNI        CTB(X)
159600140428      * x gestire dei codici particolari al posto di quelli standard devo memorizzare
159700140428      * il codice del tipo bolla (1 byte) + l'eventuale suffisso (2 bytes)
159800140428      * nel codice di 3 bytes ho la chiave da comparare a programma + blank(std) o
159900140428      * l'eventuale suffisso x il Partner da gestire in modo differente.
160000140428     C                   eval       CTBkey(X) = %subst(tabUNI:1:1) + due_alfa
160100140428     c***********        end
160200960530     C                   END
160300960530     C     KCOD1         READE     EDTAB01L                               30
160400960530     C                   END
160500960726     C* Caricamento NUMERATORE
160600000327     C                   clear                   X                 3 0
160700960726     C                   Z-ADD     1             §MSNUM
160800960726     C                   MOVEL     'MS'          KCOD1
160900000327     C                   MOVEL(P)  '785'         KKEY1
161000000327     C     KTABE         CHAIN     EDTAB01L
161100000327     C                   if        %found and TABFLG = *BLANKS
161200960726     C                   MOVEL     TABUNI        DSMS
161300960726     C                   ADD       1             §MSNUM
161400061215      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
161500061215      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
161600061215      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
161700061215      *  Attenzione per gestire Pracht che suddivide il messaggio in 4 occorre prevedere
161800061215      *  un salto di numerazione per evitare problemi.
161900061215     c                   move      §MSNUM        campo_4n          4 0
162000061215     c                   if        campo_4n >= 9996
162100061215     c                   add       5             §MSNUM
162200061215     c                   end
162300960726     C                   Z-ADD     WOGGI         §MSDAT
162400960726     C                   MOVEL     DSMS          TABUNI
162500960726     C                   UPDATE    EDTAB000
162600960726     C                   END
162700000327      * Caricamento Tabella codici bolla
162800000327     C                   clear                   X
162900000327     C                   MOVEL     'FF'          KCOD1
163000000327     C     KCOD1         setll     EDTAB000
163100000327     C     KCOD1         reade     EDTAB000
163200000327     C                   DOW       not %eof
163300010220     C                   If         TabFLG = *BLANKS
163400000327     C                   ADD       1             X
163500010220     C                   MOVEL     TabKEY        CFF(X)
163600010220     C                   MOVEL     TabUNI        DSFF
163700000327     C                   MOVEL     §FFIDN        DFF(X)
163800000327     C                   ENDif
163900000327     C     KCOD1         reade     EDTAB000
164000000327     C                   ENDDO
164100960530     C* Caricamento Tabella codici bolla
164200960530     C                   Z-ADD     0             X                 3 0
164300960530     C                   MOVEL     '3A'          KCOD
164400960530     C                   Z-ADD     1             KKUT
164500960530     C     KTAB          CHAIN     TABEL00F                           30
164600960530     C     *IN30         DOWEQ     '0'
164700960530     C     TBLFLG        IFEQ      *BLANKS
164800960530     C                   ADD       1             X
164900960530     C                   MOVEL     TBLKEY        C3A(X)
165000960530     C                   MOVEL     TBLUNI        D3A(X)
165100960530     C                   END
165200960530     C     KTAB          READE     TABEL00F                               30
165300960530     C                   END
165400960822     C* Caricamento Tabella codici nazioni
165500960822     C                   Z-ADD     0             X                 3 0
165600960822     C                   MOVEL     '15'          KCOD
165700960822     C                   Z-ADD     1             KKUT
165800960822     C     KTAB          CHAIN     TABEL00F                           30
165900960822     C     *IN30         DOWEQ     '0'
166000960822     C     TBLFLG        IFEQ      *BLANKS
166100960822     C                   ADD       1             X
166200960822     C                   MOVEL     TBLKEY        C15(X)
166300960822     C                   MOVEL     TBLUNI        DS15
166400960822     C                   MOVEL     §15COD        ISO(X)
166500170418     C                   MOVEL     §15PREF       prf(X)
166600960822     C                   END
166700960822     C     KTAB          READE     TABEL00F                               30
166800960822     C                   END
166900960530     C* Caricamento Tabella CV
167000960530     C                   MOVEL     'CV'          KCOD
167100960530     C     KTAB          CHAIN     TABEL00F                           30
167200960530     C     *IN30         DOWEQ     '0'
167300960530     C     TBLFLG        IFEQ      *BLANKS
167400960530     C                   MOVEL     TBLUNI        DSCV
167500960530     C     §CVITA        IFEQ      'I'
167600960530     C                   MOVEL     TBLKEY        WDIVIT            3
167700960530     C                   END
167800960530     C                   END
167900960530     C     KTAB          READE     TABEL00F                               30
168000960530     C                   END
168100961126     C* Reperisco Kg per metro cubo per calcolo peso tassabile
168200961126     C                   MOVEL     '7A'          KCOD
168300961126     C                   MOVEL(P)  '2'           KKEY
168400961126     C     KTAB1         CHAIN     TABEL00F                           31
168500961126     C  N31              MOVEL     TBLUNI        DS7A2
168600961126     C   31              CLEAR                   DS7A2
168700150825     C*
168800150825     C** CARICAMENTO DATI VARI TASSAZIONE
168900150825     C                   Z-ADD     1             KKUT
169000150825     C                   MOVEL     'QT'          kcod
169100150825     C                   MOVEL(P)  '1'           kKEY
169200150825     C     KTAB1         CHAIN     TABEL00F                           31
169300150825     C  N31              MOVEL     TBLUNI        DSQT1
169400150825     C   31              CLEAR                   DSQT1
169500150825     C*
169600150825      *------------
169700960530     C* finta read x IFCSUM
169800960530     C   01
169900960530     CANN01              READ      EDIFCSUM                               01
170000970124     C*
170100970204     C***                  Z-ADD56        LUNG
170200970204     C***                  MOVELCMD,1     COMMAN
170300970204     C***                  CALL 'QCMDEXC'              90
170400970204     C***                  PARM           COMMAN 80
170500970204     C***                  PARM           LUNG   155
170600970204     C***                  OPEN FNARB01L
170700010424     C*
170800960530     C*
170900960530     C                   ENDSR
171000121105      *---------------------------------------------------------------*
171100121105      * CTRL caricamento schiere    PT                               -*
171200121105      *---------------------------------------------------------------*
171300121105     C     Check_PT      begsr
171400121105     C*
171500121105     c* Controllo riempimento schiera
171600121105     c                   clear                   trul0sds
171700121105     c                   eval      i0sski='CPT'
171800121105     c                   eval      i0sele=%elem(CPT)
171900121105     c                   eval      i0spie=x
172000121105     c                   eval      i0sfile='EDTAB00F'
172100121105     c                   eval      i0ssif=knsif
172200121105     c                   eval      i0spgm='TRTC83R2'
172300121112     c                   movel     kpjbu         kpjbusav
172400121112     c                   clear                   kpjbu
172500121105     c                   movel     trul0sds      kpjbu
172600121105     c                   call      'TRUL0SR'
172700121105     c                   parm                    kpjba
172800121112     c                   movel     kpjbusav      kpjbu
172900121105     C*
173000121105     C                   ENDSR
173100170418     C*****************************************************************
173200170418     C*  Compatta il campo numero di telefono avvicinando i numeri
173300170418     C*****************************************************************
173400170418     C     compatta_NR   BEGSR
173500170418      *
173600170418     c                   z-add     0             in                3 0
173700170418     c                   clear                   Byte_uno          1
173800170418     c                   clear                   out16            16
173900170418      *
174000170418      *  controlla presenza di caratteri divisori o blank
174100170418      *   li elimina e deve compattare il numero
174200170418     c                   do        16            Nx                3 0
174300170418     c                   eval      Byte_uno = %subst(input16:Nx:1)
174400170418      * controlla che NON sia un numero :
174500170418     c                   move      'N'           alfa              1
174600170418     c     digits        check     Byte_uno
174700170418     c                   if        %found
174800170418     c                   move      'S'           alfa
174900170418     c                   end
175000170418      *
175100170418      * tranne sul primo che può esserci il '+'
175200170418     c                   if        Nx = 1 and Byte_uno ='+'
175300170418     c                             or alfa = 'N'
175400170418     c                   add       1             in
175500170418     c                   eval      %subst(out16:in:1) = %subst(input16:Nx:1)
175600170418     c                   end
175700170418     c                   enddo
175800170418      *
175900170418     c                   endsr
176000170418     C*****************************************************************
176100960530     OFNARB000  E            AGGTRA
176200960530     O                       ARBFT3
176300960530     O                       ARBDT3
