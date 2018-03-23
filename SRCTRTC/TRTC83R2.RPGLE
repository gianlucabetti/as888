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
002700171018     Ffipnd01L  IF   E           K DISK
002800120119     FFNorm01L  IF   E           K DISK
002900050309     FTIGCP02L  IF   E           K DISK
003000960726     FEDTAB01L  UF   E           K DISK
003100960530     FTABEL00F  IF   E           K DISK
003200960530     FEDIFCSUM  UF A E             DISK
003300000207      *---------------------------------------------------------------------*
003400000207      * Schiere
003500000207      *---------------------------------------------------------------------*
003600000207      * Schiera per reperimento importo/valuta tipo/qualif.imp.
003700000207      * Schiera per reperimento testo libero singola sped.
003800000207      * Schiera per reperimento nr.riferimento spedizione
003900000207      * Schiera per reperimento persona a cui è diretta comunic.
004000000207      * Schiera per reperimento nr.seganacollo
004100000207      * Priorità trasporto
004200960530     D PTR             S              3    DIM(100)
004300960530     D TSP             S              1    DIM(100)
004400000207      * Termini di consegna
004500960530     D CTB             S              1    DIM(100)
004600140428     D CTBkey          S              3    DIM(100)                             ctb+suffisso
004700960712     D TCO             S              3    DIM(100)
004800000207      * Codici bolla
004900960530     D C3A             S              2    DIM(200)
005000960530     D D3A             S             89    DIM(200)
005100000207      * Codici ISO nazioni x E.D.I.
005200011217     D C15             S              3    DIM(500)
005300011217     D ISO             S              3    DIM(500)
005400170418     D PRF             S              4    DIM(500)
005500000207      * Tipo pagamento C/Assegno
005600130227     D C1A             S             10    DIM(100)
005700960716     D TPI             S              2    DIM(100)
005800130215     D TPIkey          S             35    DIM(100)
005900000327      * Codici ISO nazioni x E.D.I.
006000000327     D CFF             S              7  0 DIM(100)
006100000327     D DFF             S             35    DIM(100)
006200010424     D* Tabella partner
006300121105     D CPT             S             35    DIM(200)
006400121105     D DPT             S             90    DIM(%elem(CPT))
006500121105     D KSC             S              7  0 DIM(%elem(CPT))
006600121105     D DPU             S             90    DIM(%elem(CPT))
006700121105     D DPv             S             90    DIM(%elem(CPT))
006800121105     D DPz             S             90    DIM(%elem(CPT))
006900170412     D DPs             S             90    DIM(%elem(CPT))
007000121105     D TRUL0SDS      E DS
007100150825     D DSQT1         E DS                  INZ
007200000207      *---------------------------------------------------------------------*
007300000207      * DS
007400000207      *---------------------------------------------------------------------*
007500000207      * .. per scompozione dati ricevuti a seconda del tipo record
007600960530     D EDTD00        E DS                  EXTNAME(EDTD00DS)
007700000207     D  D00                   77    142    DIM(3)
007800960530     D EDTD05        E DS                  EXTNAME(EDTD05DS)
007900000207     D  D05                   13    362    DIM(5)
008000000207     D  D05A                 366    715    DIM(5)
008100960530     D EDTD10        E DS                  EXTNAME(EDTD10DS)
008200000207     D  D10                   73    186    DIM(3)
008300960530     D EDTD15        E DS                  EXTNAME(EDTD15DS)
008400000207     D  D15                  445    500    DIM(2)
008500960530     D EDTD20        E DS                  EXTNAME(EDTD20DS)
008600960530     D EDTD25        E DS                  EXTNAME(EDTD25DS)
008700960530     D EDTD30        E DS                  EXTNAME(EDTD30DS)
008800000207     D  D30                   10    709    DIM(20)
008900960530     D EDTD35        E DS                  EXTNAME(EDTD35DS)
009000960530     D EDTD40        E DS                  EXTNAME(EDTD40DS)
009100010424      *-------
009200010424      *  Ds per reperimento tabella PT e campo risultato
009300010424     D DSPT          E DS                  EXTNAME(EDIDSPT)
009400010424     D Partner         S             35
009500010730     D DSPU          E DS                  EXTNAME(EDIDSPU)
009600120119     D DSPV          E DS                  EXTNAME(EDIDSPV)
009700120705     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
009800170412     D DSPS          E DS                  EXTNAME(EDIDSPS)
009900010424      *-------
010000000207      *  Ds per reperimento dati importi
010100960530     D WTD00           DS
010200960530     D  WTPI                   1      3
010300960530     D  WIMP                   4     19  3
010400960530     D  WVAL                  20     22
010500000207      *  Ds per reperimento riferimeti singola spedizione
010600960530     D WTD10           DS
010700960530     D  WTPR                   1      3
010800960530     D  WRIF                   4     38
010900000207      * DS per reperimento destinatario comunicazione
011000960530     D WTD15           DS
011100960530     D  WTCD                   1      3
011200960530     D  WCMD                   4     28
011300000207      *---------------------------------------------------------------------*
011400000207      * .. per reperimento tipo record di EDIFTMIN letto
011500000207      *                    .. 5 - 8  : tipo record letto
011600960705     D WDAT            DS          1950
011700960726     D  SUMPRG                 1      4  0
011800960530     D  SUMTPR                 5      8
011900000207      *---------------------------------------------------------------------*
012000000327     D DSFF          E DS                  EXTNAME(EDIDSFF)
012100000327     D DSMS          E DS                  EXTNAME(EDIDSMS)
012200960530     D TRTC82        E DS                  EXTNAME(TRTC82DS)
012300171018     D**** dsBL4i        E DS
012400090223     D dsBL4a        E DS
012500120119     D dsBL4m        E DS
012600120119     D dsBL4e        E DS
012700960530     D DS3A          E DS
012800961126     D DS7A2         E DS
012900960822     D DS15          E DS
013000960530     D DSCV          E DS
013100031210     D dar5gen       e ds
013200170418     D dar5emd       e ds
013300170418     d  fldSMS         s                   like(§ar5TEL)
013400170418     d  fldPREF        s              4A
013500170419     d  Invia_alert_mail...
013600170419     d                 s                   like(§ar5EML)
013700170419     d  Invia_alert_sms...
013800170419     d                 s             30A
013900950428     D KPJBA         E DS
014000121112     d kpjbusav        S                   like(Kpjbu)
014100960530     D WLBDA8          DS                  INZ
014200950428     D  G02DAT                 1      8  0
014300950428     D  G02INV                 9     16  0
014400950428     D  G02ERR                17     17
014500950428     D  G02TGI                18     22  0
014600960530     D*  DS x scomposizione numero spedizione
014700960530     D DSSPE           DS
014800960530     D  ARBAAS                 1      4  0
014900960530     D  ARBLNP                 5      7  0
015000960530     D  ARBNRS                 8      9  0
015100960530     D  ARBNSP                10     16  0
015200960905     D  SPE01                  3     16  0
015300960530     D*  DS x scomposizione segnacollo
015400960530     D DSSGN           DS
015500961029     D  ARTFLS                 1      3  0
015600960530     D  ARTLNA                 4      6  0
015700960530     D  ARTNRS                 7      8  0
015800960530     D  ARTNSC                 9     15  0
015900960829     D  ARTZNC                16     17  0
016000970203     D                 DS
016100970203     D  TESTO1                 1      5
016200970203     D  GCAAGC                 6      7  0
016300970203     D  GCAFGC                 9     11  0
016400970203     D  GCANGC                13     19  0
016500970203     D  TESTO2                21     25
016600970203     D  VARIO                 27     33
016700970203     D  RIFGIA                 1     33
016800970203     D                 DS
016900970203     D  TESTO3                 1      8
017000970203     D  RAAS                   9     10  0
017100970203     D  RLNP                  12     14  0
017200970203     D  RNRS                  16     17  0
017300970203     D  RNSP                  19     25  0
017400970203     D  RIFSPE                 1     25
017500001222     D                 DS
017600001222     D  chkSPE                 9     25
017700001222     D  dgtSPE                 1     25
017800050518      **
017900050518     d  fld15a         s             15
018000050518     D  a15            s              1    DIM(15)
018100120119      **------
018200120119     D Richiesta_sostituzione_CU...
018300120119     d                 s              1
018400120119     D Bolla_di_Reso...
018500120119     d                 s              1
018600120119     D Bolla_da_ORM...
018700120119     d                 s              1
018800120119     D Rif_RESO_Orig...
018900120119     d                 s             35
019000120119     D Rif_ORM_Orig...
019100120119     d                 s             35
019200120119     d  Qualificatore_RESO...
019300120119     d                 s              3
019400120119     d  Qualificatore_ORM...
019500120119     d                 s              3
019600120119     d  Tipo_Rif_RESO...
019700120119     d                 s              1
019800120119     d  Tipo_RIf_ORM...
019900120119     d                 s              1
020000120330     d  Qualificatore_PCI...
020100120330     d                 s              3
020200120330     d  Quanti_x_PCI...
020300120330     d                 s              2s 0
020400120119     d  Num            s              3s 0
020500120119      **------
020600050518      **
020700970203     D*  Definisco costanti per bolle reso
020800970203     D COST1           C                   CONST('EX SPED.')
020900970203     D COST2           C                   CONST('GIAC.')
021000020711     D COST3           C                   CONST('LNP.')
021100001222     D digits          C                   CONST('0123456789')
021200150722     D GWEISS          C                   CONST('GW-SPAC                      -
021300150722     D                                           ')
021400060116     D Pracht          C                   CONST('PRACHT                       -
021500060116     D                                           ')
021600970124     I*
021700970204     I***                 CMD     1   1 56               OVRDBF
021800960530     I*---------------------------------------------------------------------*
021900960530     I* FILE
022000960530     I*---------------------------------------------------------------------*
022100960704     IRRIFCSUM
022200960530     I              RRIFCSUM                    SUMDAT
022300960530     C*---------------------------------------------------------------------*
022400960530     C* Ciclo principale                                                   -*
022500960530     C*---------------------------------------------------------------------*
022600010426     C     *ENTRY        PLIST
022700010426     C                   PARM                    KPJBA
022800010426     C                   MOVEL     KPJBU         TRTC82
022900010426     C*
023000960530     C*  Se c'è numero spedizione scrivo dettaglio
023100970124    2C     D82NSP        IFNE      0
023200010426     C*
023300010426     C*-------------------
023400010426     C*  In base al codice del PARTNER reperisco identificativo
023500010426     C*   per decidere per chi scrivere il VOLUME subito successivo.
023600010426     C*    da utilizzare nella scrittura del TD25 dettaglio Volume.
023700010426     C                   Z-ADD     1             XX                3 0
023800010426     C                   Clear                   partner
023900010426     C     D82CPT        LOOKUP    KSC(XX)                                32
024000010426     C   32              MOVEL     CPT(XX)       partner
024100010730     C   32              MOVEL     DPU(XX)       DSPU
024200120119     C   32              MOVEL     DPV(XX)       DSPV
024300120705     C   32              MOVEL     DPZ(XX)       DSPZ
024400170412     C   32              MOVEL     DPs(XX)       DSPS
024500170412      **
024600120119     c                   eval       Richiesta_sostituzione_CU =' '
024700170412      **
024800120119      * imposta richiesta sostituzione RFF+CU con altro qualificatore
024900120119      *   per invio riferimento originale di un RESO o di un ORM.
025000120119     c                   if        §PVCURESO ='S' or §PVCUORM ='S'
025100120119     c                   eval       Richiesta_sostituzione_CU ='S'
025200120119     C                   eval         Qualificatore_RESO = §PVCUQUAR
025300120119     C                   eval         Qualificatore_ORM  = §PVCUQUAO
025400120119     C                   eval              Tipo_Rif_RESO = §PVCUTIPR
025500120119     C                   eval              Tipo_RIf_ORM  = §PVCUTIPO
025600120119     c                   endif
025700010426     C*-------------------
025800120330      ** aggiunti x SEUR  sul segm. PCI il qualificatore da Inviare e quanti
025900120330      ** Barcode per segmento PCI devono essere inviati
026000120330     C                   eval         Qualificatore_PCI  = §PVquaPCI
026100120330      *
026200120330      * inizializza a 20 la schiera dei codici a barre
026300120330     C                   z-add     20            Quanti_x_PCI
026400120402     c                   if            §PVpciXseg <> *blank and
026500120402     c                                 §PVpciXseg <> *zeros
026600120330     C                   move      §PVpciXseg    Quanti_x_PCI
026700120330     c                   end
026800120330     C*-------------------
026900960530     C*  Eseguo posizionamento su archivio bolle per passaggio param.
027000960530     C                   Z-ADD     D82AAS        KAAS
027100960530     C                   Z-ADD     D82LNP        KLNP
027200960530     C                   Z-ADD     D82NRS        KNRS
027300960530     C                   Z-ADD     D82NSP        KNSP
027400960530     C     KARB          CHAIN     FNARB01L                           31
027500970124    3C     *IN31         IFEQ      *OFF
027600120119      *
027700120119     c                   eval         Bolla_da_ORM = *blank
027800120119     c                   eval         Bolla_di_RESO= *blank
027900120119      *****
028000120119      **** se bolla di reso
028100120119      ****     x sostituzione riferimento per il Partner che lo richiede
028200120119     c                   if        ARBFBR = 'S'  and §PVCURESO ='S'
028300120119     C     KARB          CHAIN     FNlbl01L
028400120119    3C                   IF           %Found(FNlbl01L)
028500120119     c                   eval         Bolla_di_RESO = 'S'
028600120119     c                   end
028700120119     c                   end
028800120119      *****
028900120119      **** se bolla da ORM
029000120119      ****     x sostituzione riferimento per il Partner che lo richiede
029100120119     c                   if         §PVCUORM ='S'
029200120119     C                   movel     'M'           KTRC
029300120119     C     KAR4          CHAIN     FiAR401L
029400120119    3C                   IF           %Found(FiAR401L)
029500120119     c                   movel     AR4NOT        dsBl4m
029600120119     c                   if        §B4POE = arbLNA
029700120119     c                   eval         Bolla_da_ORM = 'S'
029800120119     c                   end
029900120119     c                   end
030000120119     c                   end
030100170418      **-----------
030200170418      **   abilitato all'invio dell'Alert (Predict) Email o SMS
030300170418     c                   eval      Invia_alert_mail = ' '
030400170418     c                   eval      Invia_alert_sms  = ' '
030500170412      **
030600170418      **  x inviare l'Alert  (tab.PS)
030700170418     c                   if         §PSSALERT ='S'
030800170419     c                   clear                   fldPREF
030900170419     c                   clear                   fldSMS
031000170418      *-
031100170418     c                   movel     'EMD'         ktrd
031200170418     c                   clear                   DAr5emd
031300170418      *  EMAIL o SMS - come servizio di Alert di consegna -
031400170418     c     kar5          chain     fiar501L
031500170418     c                   if        %Found(fiar501L)
031600170418     c                   Movel     Ar5Uni        DAr5emd
031700170419     c                   eval      Invia_alert_mail = §ar5EML
031800170419     c                   movel     §ar5TEL       input16          16
031900170418     c                   exsr      compatta_NR
032000170418     c                   movel     out16         fldSMS
032100170418      * se c'è già il prefisso
032200170418     c                   if        %subst(fldSMS:1:1) = '+' or
032300170418     c                             %subst(fldSMS:1:2) = '00'
032400170419     c                   eval      Invia_alert_sms    = fldSMS
032500170418     c                   else
032600170418      *  altrimenti con la
032700170418     C* Nazione Destinatario imposta il Prefisso nel numero
032800170418     C                   Z-ADD     1             YY
032900170418     C     arbNZD        LOOKUP    C15(YY)                                32
033000170418     C  N32D82NAZ        LOOKUP    C15(YY)                                32
033100170418      *   se c'è il pref.internazionale in tabella Nazioni
033200170418      *   imposta il codice preceduto dal '+'
033300170418     c   32              if        PRF(YY) <> *blank
033400170418     C                   MOVEL     PRF(YY)       fldPREF
033500170418     c                   end
033600170418      *
033700170418      * se non c'è il prefisso lo imposta solo se presente sulla tab.15
033800170418     c                   if        fldPREF <> *blank and fldSMS <> *blank
033900170419     c                   eval       Invia_alert_sms = '+'
034000170418     c                               + %Trim(fldPREF) + %Trim(fldSMS)
034100170418     c                   end
034200170418     c                   endif
034300170418     c                   end
034400170418      *-
034500170418     c                   endIF
034600170418     C*----------------
034700170418      *
034800960530     C*  Scrittura D00
034900960530     C                   EXSR      WRTD00
035000960530     C*  Scrittura D05
035100960530     C                   EXSR      WRTD05
035200170419     C*  Scrittura D05 x Alert
035300170419     c                   if          invia_alert_SMS  <> *blank  or
035400170419     c                               invia_alert_Mail <> *blank
035500170419     C                   EXSR      WRTD05_alert
035600170419     c                   end
035700960530     C*  Scrittura D10
035800960530     C                   EXSR      WRTD10
035900960530     C*  Scrittura D15
036000960530     C                   EXSR      WRTD15
036100960530     C*  Scrittura D20
036200960530     C                   EXSR      WRTD20
036300960530     C*  Scrittura D25
036400960530     C                   EXSR      WRTD25
036500960827     C*  Scrittura D30 solo se lo vogliono
036600980408    4C     D82DET        IFNE      *BLANKS
036700960530     C                   EXSR      WRTD30
036800970124    4C                   END
036900960827     C*
037000970124    3C                   END
037100960530     C*  Fleggo FNARB come trasmesso
037200960530     C                   MOVEL     'T'           ARBFT3
037300961126     C                   Z-ADD     D82DFV        ARBDT3
037400960530     C                   EXCEPT    AGGTRA
037500970124     C                   SETON                                        RT
037600970124     C*
037700970124   X2C                   ELSE
037800060116     C*
037900960530     C*  Se non c'è spedizione: ultima volta --> Scrivo totali in DS
038000960726     C                   Z-ADD     §MSNUM        D82PRG
038100960530     C                   Z-ADD     WTONCL        D82TNC
038200960530     C                   Z-ADD     WTOPKF        D82TPK
038300960530     C                   Z-ADD     WTOVLF        D82TVL
038400960530     C                   Z-ADD     WPRG          D82TSP
038500960705     C                   MOVEL     TRTC82        KPJBU
038600060116      **
038700060116 ?    **  Il programma se ha fatto l'EDI x la PRACHT incrementa di altri 3 numeri il
038800060116 ?    **   numeratore x un totale di 4 incrementi(considerando anche quello fatto nella
038900060116 ?    **   INZSR)e aggiorna la tabella numeratori x un manifest di altro PARTNER.
039000060116 ?    **  Questo accade perchè il msg della PRACHT deve essere diviso in 4 EDI separati.
039100060116 ?    **   La numerazione dei msg PRACHT viene rivista nei pgm seguenti x l'invio dell'EDI.
039200060116      **
039300060116     c                   if        Partner = PRACHT
039400060116      **
039500060116 ?    **  Aggiornamento NUMERATORE solo x PRACHT
039600150722      **   poichè aveva 4 messaggi separati.
039700060116     C                   Z-ADD     1             §MSNUM
039800060116     C                   MOVEL     'MS'          KCOD1
039900060116     C                   MOVEL(P)  '785'         KKEY1
040000060116     C     KTABE         CHAIN     EDTAB01L
040100060116     C                   if        %found and TABFLG = *BLANKS
040200060116     C                   MOVEL     TABUNI        DSMS
040300060116     C                   ADD       3             §MSNUM
040400060116     C                   Z-ADD     WOGGI         §MSDAT
040500060116     C                   MOVEL     DSMS          TABUNI
040600060116     C                   UPDATE    EDTAB000
040700060116     C                   END
040800060116      **
040900060116     c                   end
041000060116      **
041100970124     C                   SETON                                        LR
041200970124    2C                   END
041300960530     C*--------------------------------------------------------------*
041400960530     C* WRTD00: scirvo tipo record D00 in IFCSUM                    -*
041500960530     C*--------------------------------------------------------------*
041600960530     C     WRTD00        BEGSR
041700960530     C*
041800960530     C                   CLEAR                   EDTD00
041900960702     C                   CLEAR                   AR9TIC
042000960702     C                   CLEAR                   AR9CAS
042100960530     C*  Imposto nr.spedizione
042200960530     C                   ADD       1             WPRG              7 0
042300960530     C                   Z-ADD     WPRG          DA1490
042400960906     C     D82CNI        IFLE      15
042500960905     C     D82SEC        ANDEQ     'N'
042600960905     C                   MOVEL     SPE01         DA1004
042700960905     C                   ELSE
042800960905     C                   MOVEL     DSSPE         DA1004
042900960905     C                   END
043000960530     C                   Z-ADD     9999          DA1312
043100150722      ***
043200150722      * chiodo:
043300150722      *** ??????? ---------------------------------------------------
043400150722      ***  CHIODO per GWEISS la Data Consegna Richiesta deve essere inviata
043500150722      *** ???????     solo per il cliente KIKO 0934683  -  Altrimenti NO
043600150722      ***  *xché la sped.costerebbe il 30% in più*
043700150722      ***     (Solfrini - Santini - Bocchi - Delledonne)
043800150722     C     Partner       IFeq      GWEISS
043900150722     c                   if          ARBKSC <> 934683
044000150722     c                   z-add     0             arbdcr
044100150722     c                   end
044200150722     c                   end
044300150722      *** ??????? ---------------------------------------------------
044400150722      ***
044500960530     C*  Se c'è scrivo data consegna richiesta
044600960530     C     ARBDCR        IFNE      0
044700960530     C                   MOVEL     'Z73'         DA2005
044800960530     C                   MOVEL     ARBDCR        DA2380
044900960530     C                   MOVEL     '102'         DA2379
045000960530     C                   END
045100031209      *
045200960530     C*  Recupero il tipo spedizione dal tipo servizio
045300960726     C**                   Z-ADD1         X
045400960726     C**         ARBTSP    LOKUPTSP,X                    32
045500960726     C**         *IN32     IFEQ '1'
045600960726     C**                   MOVELPTR,X     DA4219
045700960726     C**                   END
045800031209      *
045900020123      *
046000031209      * ? Particolarità TSR da attivare dal 1.01.2004
046100031209      *  -----------------------------------------------------------------
046200031209      *  Pre-noon serv -> Espresso                        TSR+++3+PRN
046300031209      *  Envelopes     -> busta di plastica 1 Kg.         TSR+++3+ENV
046400031209      *  Booking ins   -> Appuntamento (+ data DTM Z73)   TSR+++3+BKI
046500031209      *  Saturday serv -> Consegna di sabato (non util.)  TSR+++3+SPS
046600120705      *  GoodWaitCollect> FERMO DEPOSITO se abilitato PT  TSR+++3+GWC
046700031209      *  -----------------------------------------------------------------
046800031209      * Espresso consegna prima di mezzogiorno
046900091014     c                   if        ArbTSP = 'E'  or
047000091014     c                             ArbTSP = 'H'
047100031209     C                   eval      DA4219 = '3  '
047200031209     C                   eval      DA7085 = 'PRN'
047300031209     c                   end
047400031209      *
047500031209      * Prenotazione su Appuntamento
047600031209      *  + data prevista consegna comunque precedentemente impostata
047700031209     c                   if        ArbTC1 = 'A' or  ArbTC2 = 'A'
047800031209     C                   eval      DA4219 = '3  '
047900031209     C                   eval      DA7085 = 'BKI'
048000031209     c                   end
048100031209      *
048200031209      * Servizio di Sabato   non è gestito da Bartolini
048300031209     c*******            if        ...............
048400031209     C*******            eval      DA4219 = '3  '
048500031209     C*******            eval      DA7085 = 'SPS'
048600031209     c*******            end
048700031209      *
048800031209      * Busta di plastica con max 1Kg. non è gestita da Bartolini
048900031209     c*******            if        ...............
049000031209     C*******            eval      DA4219 = '3  '
049100031209     C*******            eval      DA7085 = 'ENV'
049200031209     c*******            end
049300031209      *
049400120705      *  FERMO DEPOSITO
049500120705     c                   if        ARBFFD = 'S' and §pzTSRgwc ='S'
049600120705     C                   eval      DA4219 = '3  '
049700120705     C                   eval      DA7085 = 'GWC'
049800120705     c                   end
049900120705      *
050000031209      *  -----------------------------------------------------------------
050100031209      *
050200960530     C*  Scrivo valore merce se c'è
050300970219     C                   Z-ADD     0             X
050400021118     C                   if        ARBVMD > 0
050500021118     C                   ADD       1             X
050600021118     C                   MOVEL     '77 '         WTPI
050700021118     C                   Z-ADD     ARBVMD        WIMP
050800960530     C*  Se divisa a blanks imposto italiana
050900021118     C                   if        ARBVAD = *BLANKS
051000021118     C                   MOVEL     WDIVIT        WVAL
051100021118     C                   else
051200021118     C                   MOVEL     ARBVAD        WVAL
051300021118     C                   end
051400021118     C                   MOVEL     WTD00         D00(X)
051500021118     C                   end
051600021118      *
051700960530     C*  Scrivo importo da assicurare se c'è
051800961115     C***        ARBIAS    IFGT 0
051900961115     C***                  ADD  1         X
052000961115     C***                  MOVEL'157'     WTPI
052100961115     C***                  Z-ADDARBIAS    WIMP
052200960530     C*  Se divisa a blanks imposto italiana
052300961115     C***        ARBVAS    IFEQ *BLANKS
052400961115     C***                  MOVELWDIVIT    WVAL
052500961115     C***                  ELSE
052600961115     C***                  MOVELARBVAS    WVAL
052700961115     C***                  END
052800961115     C***                  MOVELWTD00     D00,X
052900961115     C***                  END
053000960530     C*  Se c'è C/Assegno se c'è
053100960530     C                   Z-ADD     1             Y                 3 0
053200960530     C     ARBCBO        LOOKUP    C3A(Y)                                 32
053300960530     C     *IN32         IFEQ      '1'
053400960530     C                   MOVEL     D3A(Y)        DS3A
053500960530     C     §3AFCA        IFNE      0
053600051114     C     KAR9          CHAIN     FIAR901L                           31
053700960530     C     AR9CAS        IFGT      0
053800960530     C                   ADD       1             X
053900960705     C                   MOVEL     '22 '         WTPI
054000960530     C                   Z-ADD     AR9CAS        WIMP
054100970221     C                   MOVE      '0'           WIMP
054200960530     C*  Se divisa a blanks imposto italiana
054300960530     C     AR9VCA        IFEQ      *BLANKS
054400960530     C                   MOVEL     WDIVIT        WVAL
054500960530     C                   ELSE
054600960530     C                   MOVEL     AR9VCA        WVAL
054700960530     C                   END
054800960530     C                   MOVEL     WTD00         D00(X)
054900960530     C                   END
055000960530     C*
055100960530     C                   END
055200960530     C                   END
055300960705     C*  Scrittura record D00
055400960705     C                   CLEAR                   WDAT
055500960705     C                   MOVEL     EDTD00        WDAT
055600960726     C                   Z-ADD     §MSNUM        SUMPRG
055700960705     C                   MOVEL     'TD00'        SUMTPR
055800960705     C                   MOVEL     WDAT          SUMDAT
055900960704     C                   WRITE     RRIFCSUM
056000960530     C*
056100960530     C                   ENDSR
056200960530     C*--------------------------------------------------------------*
056300960530     C* WRTD05: scirvo tipo record D05 in IFCSUM                    -*
056400960530     C*--------------------------------------------------------------*
056500960530     C     WRTD05        BEGSR
056600960530     C*
056700960530     C                   CLEAR                   EDTD05
056800960628     C                   MOVEL     'TD05'        SUMTPR
056900960530     C*  Controllo se ci sono delle note di consegna
057000960530     C*  Reperisco note spedizione
057100960530     C                   MOVEL     '8'           KTRC
057200960530     C                   Z-ADD     0             X
057300060213     C     KAR4          CHAIN     fiar401L                           32
057400960530     C     *IN32         IFEQ      '0'
057500960530     C                   ADD       1             X
057600960530     C                   MOVEL     AR4NOT        D05(X)
057700970203     C*  Se è una bolla di reso reperisco rif. originale patner
057800970203     C*  per scriverlo nelle note
057900970203     C     ARBFBR        IFEQ      'S'
058000970203     C                   EXSR      RIFPAT
058100970203     C                   END
058200970203     C                   END
058300960530     C                   MOVEL     '9'           KTRC
058400060213     C     KAR4          CHAIN     fiar401L                           32
058500960530     C     *IN32         IFEQ      '0'
058600960530     C                   ADD       1             X
058700960530     C                   MOVEL     AR4NOT        D05(X)
058800960530     C                   END
058900960628     C*  Se ho trovato qualche nota scrivo record D05
059000960628     C     X             IFGT      0
059100960716     C                   MOVEL     'SIN'         DB4451
059200960628     C                   END
059300130215      *
059400960628     C*  Se C'è il contrassegno scrivo mod.pagamento
059500960716     C     AR9CAS        IFNE      0
059600130215      *
059700960628     C     X             IFGT      0
059800960701     C                   MOVEL     'PAI'         DBE451
059900130215      *
060000130215      * la chiave deve essere di 35 non più di solo 2 caratteri
060100130215     C                   movel     ar9TIC        field35          35
060200130215      *
060300130215      * solo se presente il suffisso per un potenziale codice Fuori Standard
060400130215      *  da inviare al posto dello standard e se però non lo trova deve
060500130215      *   vedere con lo standard.
060600130215     c                   if        §pzSUFFtab <> *blank
060700130215     C                   move      §pzSUFFtab    field35
060800130215     c                   end
060900130215      *
061000130215      *  prova con chiave completa di 35 caratteri:
061100130215     C                   Z-ADD     1             Y
061200130215     C     field35       LOOKUP    TPIkey(Y)                              33
061300130215     C**** AR9TIC ****** LOOKUP    TPI(Y)                                 33
061400130215      * se non lo ha trovato prova con la chiave standard:
061500130215     C  n33              move      *blank        Due_blanks        2
061600130215     C  n33              move      Due_blanks    field35
061700130215     C  n33field35       LOOKUP    TPIkey(Y)                              33
061800130215      *
061900970204     C   33              MOVEL     *BLANKS       DBF440
062000970204     C   33              MOVEL     C1A(Y)        DBF440
062100130215      *
062200130215      *  Se ne avesse più di uno di tipi incasso
062300960628     C                   ELSE
062400130215      *  imposta nel secondo
062500960628     C                   MOVEL     'PAI'         DB4451
062600130215      *
062700130215      * la chiave deve essere di 35 non più di solo 2 caratteri
062800130215     C                   movel     ar9TIC        field35          35
062900130215      *
063000130215      * solo se presente il suffisso per un potenziale codice Fuori Standard
063100130215      *  da inviare al posto dello standard e se però non lo trova deve
063200130215      *   vedere con lo standard.
063300130215     c                   if        §pzSUFFtab <> *blank
063400130215     C                   move      §pzSUFFtab    field35
063500130215     c                   end
063600130215      *
063700130215      *  prova con chiave completa di 35 caratteri:
063800130215     C                   Z-ADD     1             Y
063900130215     C     field35       LOOKUP    TPIkey(Y)                              33
064000130215     C**** AR9TIC *****  LOOKUP    TPI(Y)                                 33
064100130215      * se non lo ha trovato prova con la chiave standard:
064200130215     C  n33              move      *blank        Due_blanks        2
064300130215     C  n33              move      Due_blanks    field35
064400130215     C  n33field35       LOOKUP    TPIkey(Y)                              33
064500130215      *
064600960716     C   33              MOVEL     *BLANKS       DB4440
064700960716     C   33              MOVEL     C1A(Y)        DB4440
064800130215      *
064900960628     C                   END
065000960628     C                   END
065100130215      *
065200960530     C*  Se ho trovato qualche nota scrivo record D05
065300960530     C     X             IFGT      0
065400960701     C     AR9CAS        ORGT      0
065500960705     C                   CLEAR                   WDAT
065600960705     C                   MOVEL     EDTD05        WDAT
065700960705     C                   MOVEL     'TD05'        SUMTPR
065800960726     C                   Z-ADD     §MSNUM        SUMPRG
065900960705     C                   MOVEL     WDAT          SUMDAT
066000960704     C                   WRITE     RRIFCSUM
066100960530     C                   END
066200960530     C*
066300960530     C                   ENDSR
066400170419     C*--------------------------------------------------------------*
066500170419     C* WRTD05_Alert: scrivo tipo record D05 x PREDICT ALERT mail/SMS*
066600170419     C*--------------------------------------------------------------*
066700170419     C     WRTD05_Alert  BEGSR
066800170419     C*
066900170419     C                   CLEAR                   EDTD05
067000170419     C                   MOVEL     'TD05'        SUMTPR
067100170419     C*
067200170419     C*  Se deve  avvisare il Destinatario
067300170419     C*     con MAIL
067400170419     C                   IF          Invia_alert_mail <> *blank
067500170419     C                   MOVEL     'EML'         DB4451
067600170419     c                   eval      DB4440 = Invia_alert_mail
067700170419     C*     con SMS
067800170419     C                   if          Invia_alert_SMS <> *blank
067900170419     C                   MOVEL     'SMS'         DBE451
068000170419     c                   eval      DBF440 = Invia_alert_SMS
068100170419     c                   end
068200170419      *
068300170419     C                   Else
068400170419      *
068500170419      *   altrimenti con solo SMS
068600170419     C                   MOVEL     'SMS'         DB4451
068700170419     c                   eval      DB4440 = Invia_alert_SMS
068800170419     C                   endIF
068900170419      *
069000170419     C                   CLEAR                   WDAT
069100170419     C                   MOVEL     EDTD05        WDAT
069200170419     C                   MOVEL     'TD05'        SUMTPR
069300170419     C                   Z-ADD     §MSNUM        SUMPRG
069400170419     C                   MOVEL     WDAT          SUMDAT
069500170419     C                   WRITE     RRIFCSUM
069600170419     C*
069700170419     C                   ENDSR
069800960530     C*--------------------------------------------------------------*
069900120119     C* WRTD10: scrivo tipo record D10 in IFCSUM                    -*
070000960530     C*--------------------------------------------------------------*
070100960530     C     WRTD10        BEGSR
070200960530     C*
070300960530     C                   CLEAR                   EDTD10
070400960530     C                   MOVEL     '5  '         DC4055
070500960626     C                   MOVEL     'EEL'         DC1131
070600980729      *  Imposto riferimento mittente alfabetico
070700960708     C                   Z-ADD     0             X
070800050518      ** su tab.PU
070900120119      **-------      se nell'AGE non si vuole avere la spedizione Bartolini
071000960905     C     D82NSA        IFEQ      'N'
071100980729      *
071200960905     C     ARBRMN        IFNE      0
071300960708     C                   ADD       1             X
071400960708     C                   MOVEL     'AGE'         WTPR
071500060505     C                   MOVEL(P)  ARBRMN        WRIF
071600960708     C                   MOVEL     WTD10         D10(X)
071700050518      *
071800960708     C                   ELSE
071900980729      *
072000960708     C     ARBRMA        IFNE      *BLANKS
072100960708     C                   ADD       1             X
072200960708     C                   MOVEL     'AGE'         WTPR
072300060505     C                   MOVEL(P)  ARBRMA        WRIF
072400960708     C                   MOVEL     WTD10         D10(X)
072500960708     C                   END
072600960906     C                   END
072700050518      **-------
072800960905     C                   ELSE
072900120119      *
073000120119      **-------  se nell'AGE si deve impostare la spedizione Bartolini
073100960905     C                   ADD       1             X
073200960905     C                   MOVEL     'AGE'         WTPR
073300120119      *
073400960905     C     D82AGE        IFLE      15
073500060505     C                   MOVEL(P)  SPE01         WRIF
073600960905     C                   ELSE
073700060505     C                   MOVEL(P)  DSSPE         WRIF
073800960905     C                   END
073900050518      **
074000960905     C                   MOVEL     WTD10         D10(X)
074100050518      **
074200050518      **  Se c'è anche un riferimento numerico del cliente
074300050518     C                   if        arbRMN <> 0
074400050518     C                   add       1             x
074500050518     C                   Movel     'CU '         wTPR
074600060110      *
074700060110      * ?------------------------------------------------------------------ */
074800060110      * ?Attenzione: per la trasmissione DPD occorre impostare il Parcel Nr.
074900060110      * ?  x SEUR quindi il tipo trasmissione x DPD è --> "S" deve prendere
075000060110      * ?  il Parcel dal FIAR4 con tipo record "F" i primi 12 caratteri
075100060110      * ?------------------------------------------------------------------ */
075200060110     c                   if        D82ftt ='S'
075300171018      ****
075400171018     C*****              MOVEL     'I'           KTRC
075500171018     C**** kar4          CHAIN     fiar401L
075600171018     C*****              if        %Found(fiar401L)
075700171018      ****
075800171018      ** tolto FIAR4 "I" e sostituito con il FIPND per reperire il PArcel x SEUR DPD
075900171018     C     kpnd          CHAIN     fipnd01L
076000171018     C                   if        %Found(fipnd01L)
076100060110     c                   clear                   fld15a
076200171018     c                   movel     pndIPN        fld15a
076300171018      *
076400171018     c******             movel     AR4NOT        dsBl4i
076500171018     c******             movel     §B4IPN        fld15a
076600060622     C******             eval      fld15a = %subst(ar4NOT:1:12)
076700171018      ****
076800060622     c******             else
076900060622      **** altrimenti prova con il riferimento numerico come fa normalmente.
077000060622     c******             move      arbRMN        fld15a
077100060622      *
077200060110     c                   end
077300060110      *
077400120119      * ?  x non DPD ossia in ogni altro caso
077500060110     c                   else
077600050518     c                   move      arbRMN        fld15a
077700060110     c                   end
077800060110      *
077900050518     c                   movea     fld15a        a15
078000050518     c                   do        15            ay                3 0
078100050518     c                   if        a15(ay) <> '0'
078200050518     c                   leave
078300050518     c                   End
078400050518     c                   Enddo
078500070212      * se trasm SEUR
078600070212     c                   if        D82ftt ='S'
078700070212     c                   eval      wRIF = %subst(fld15a: 1: 14)
078800070212     c                   else
078900070212      * se altro
079000050518     c                   eval      wRIF = %subst(fld15a: ay: (16-ay))
079100070212     c                   end
079200050518      **
079300050518     C                   Movel     wTD10         d10(x)
079400050518     C                   End
079500050518      **
079600960905     C                   END
079700120119      **------- -----------------------------------------------
079800120119      ** al POSTO del riferimento RFF+CU viene inviato il riferimento alternativo
079900120119      **   se è stato chiesto:
080000120119      **     per una bolla generata da un ORM, l'invio del riferimento originale
080100120119      **     alfanumerico dell'ORM
080200120119      **        oppure
080300120119      **     per una bolla generata da un RESO, l'invio del riferimento originale
080400120119      **     alfanumerico presente sulla bolla di IMPORT
080500120119      **
080600120119     c                   IF         Richiesta_sostituzione_CU ='S' and
080700120119     c                               (Bolla_di_RESO = 'S' or Bolla_da_ORM = 'S')
080800120119      **
080900120119      * se l'ultimo riferimento impostato NON era il CU occorre incrementare
081000120120     C                   if        wTPR <>'CU '
081100120119     C                   add       1             x
081200120119     c                   end
081300120120      *
081400120120      ** se è un Reso prevale sull'ORM
081500120119     c                   if        Bolla_di_RESO = 'S'
081600120119     C                   Z-ADD     bolAAO        KAAP
081700120119     C                   Z-ADD     bolLPO        KLPP
081800120119     C                   Z-ADD     bolNRO        KNRP
081900120119     C                   Z-ADD     bolNSO        KNPP
082000120119     C                   MOVEL     'E'           KTRC
082100120119     C     KBL4          CHAIN     Fiar401L
082200120119     c                   if           %Found(Fiar401L)
082300120120     c                   eval       dsbl4e = ar4not
082400120119     c                   eval         wRIF = §B4ERP
082500120120     C                   if         Qualificatore_RESO <> *blank
082600120119     C                   eval         wTPR = Qualificatore_RESO
082700120120     c                   end
082800120119      **
082900120119      **  se il riferimento deve essere numerico
083000120119     C                   if          Tipo_Rif_RESO ='N'
083100120119     c                   eval      num = %len(%Trim(wrif))
083200120119     c                   do        num
083300120119     c                   if        %subst(wrif:1:1)<>'0'
083400120119     c                   leave
083500120119     c                   else
083600120119     c                   eval      wrif = %subst(wrif:2:%len(wrif))
083700120119     c                   endif
083800120119     c                   enddo
083900120119     c                   end
084000120120      * solo se presente il riferimento
084100120120     c                   if        wrif <> *blank
084200120119     C                   movel     wTD10         d10(x)
084300120119     c                   end
084400120120     c                   end
084500120119      **
084600120119     c                   eLSe
084700120119      **
084800120119      ** se Non è un Reso ed è un ORM
084900120119     c                   if        Bolla_da_ORM = 'S'
085000120119     C                   Z-ADD     §B4POE        ORMPOE
085100120119     C                   Z-ADD     §B4NSR        ORMNSR
085200120119     C                   Z-ADD     §B4NOR        ORMNOR
085300120119     C                   Z-ADD     §B4NRV        ORMNRV
085400120119     C     Korm          CHAIN     Fnorm01L
085500120119     c                   if           %Found(Fnorm01L)
085600120120     C                   if         Qualificatore_ORM <> *blank
085700120119     C                   eval         wTPR = Qualificatore_ORM
085800120120     c                   end
085900120119     c                   eval         wRIF = ormRFA
086000120119      **
086100120119      **  se il riferimento deve essere numerico
086200120119     C                   if          Tipo_RIf_ORM ='N'
086300120119     c                   eval      num = %len(%Trim(wrif))
086400120119     c                   do        num
086500120119     c                   if        %subst(wrif:1:1)<>'0'
086600120119     c                   leave
086700120119     c                   else
086800120119     c                   eval      wrif = %subst(wrif:2:%len(wrif))
086900120119     c                   endif
087000120119     c                   enddo
087100120119     c                   end
087200120120      * solo se presente il riferimento
087300120120     c                   if        wrif <> *blank
087400120119     C                   movel     wTD10         d10(x)
087500120119     c                   end
087600120120     c                   end
087700120119      **
087800120119     c                   end
087900120119     c                   endIF
088000120119      **
088100120119     c                   end
088200120119      **------- -----------------------------------------------
088300000327      *  Scrivo segmento "FF" per Mittenti particolari
088400000327     C                   eval      yy = 1
088500000327     C     arbksc        lookup    CFF(YY)                                30
088600000327     C                   IF         *in30 = *on
088700000327     C                   add       1             X
088800000327     C                   movel     'FF '         WTPR
088900000327     C                   movel(P)  DFF(YY)       WRIF
089000000327     C                   movel     WTD10         D10(X)
089100000327     C                   ENDIF
089200980729     C*
089300960530     C*  Reperisco da EDTAB codice bolla corrispondente
089400960530     C                   Z-ADD     1             X
089500961125     C* Se franco frontiera imposto 'A' come se fosse un porto
089600961125     C* assegnato
089700960530     C                   MOVEL     §3ATB1        WPOR              1
089800961125     C     ARBFBR        IFEQ      'F'
089900961125     C                   MOVEL     'A'           WPOR
090000961125     C                   END
090100961204     C     §3ATB1        IFEQ      'FS'
090200961204     C                   MOVEL     'A'           WPOR
090300961204     C                   END
090400961204     C     §3ATB1        IFEQ      'AS'
090500961204     C                   MOVEL     'F'           WPOR
090600961204     C                   ENDIF
090700090223      ***
090800090223      ***  Attenzione:
090900090223      ***   se bolla in assegnato, controllare se già tassato e AR6KSC è
091000090223      ***  un cliente italia dire al partner che è franco
091100090223      ***  oppure
091200090223      ***  con il cliente preso dal FIAR4 con Tip.Rec."A"
091300090223      ***  se è un cliente italia dire al partner che è franco
091400090223      ***
091500090223     C                   if          WPOR = 'A'
091600090223     C     KAR6          chain     fiar601l
091700090223      * se trovato già fatturato come cliente Italia
091800090223     c                   if        %Found(fiar601l) and ar6KSC <= 2999999
091900090223     c                             and ar6KSC >0
092000090223     C                   eval        WPOR = 'F'
092100090223     C                   endIF
092200090223      *
092300090223      * altrumenti se proposto come cliente Italia
092400090223     c                   if         WPOR = 'A' or not %Found(fiar601l)
092500090223     C                   MOVEL     'A'           KTRC
092600090223     C     KAR4          CHAIN     fiar401L                           32
092700090223     c                   if        %Found(fiar401L)
092800090223     c                   eval      dsbl4a = ar4NOT
092900090223      * se Italia
093000090223     c                   if        §B4AKS <= 2999999 and §B4AKS >0
093100090223     C                   eval        WPOR = 'F'
093200090223     C                   endIF
093300090223     C                   endIF
093400090223     C                   endIF
093500090223     C                   endIF
093600090223      ***
093700140428      ***  se il partner ha un suffisso particolare, occorre verificare
093800140428      ***  se vi sono codici da trascodificare per lui prima di vedere lo standard
093900140428      ***  cosa prevede.
094000140428     c                   clear                   porto_suff        3
094100140428     c                   movel     Wpor          porto_suff
094200140428     c                   move      §pzSUFFtab    porto_suff
094300140428     C     porto_suff    LOOKUP    CTBkey(X)                              32
094400140428      *
094500140428      ***  se NON trova cerca come standard
094600140428     C*****WPOR          LOOKUP    CTB(X)                                 32
094700140428     c  n32              clear                   porto_suff
094800140428     c  n32              movel     Wpor          porto_suff
094900140428     C  n32porto_suff    LOOKUP    CTBkey(X)                              32
095000140428      *
095100960530     C     *IN32         IFEQ      '1'
095200960530     C                   MOVEL     TCO(X)        DC4053
095300960530     C                   END
095400960530     C* Dati traporto
095500060118     C******             MOVEL     '20 '         DC8051
095600060118     C******             MOVEL     '3  '         DC8067
095700060118      * ?***  Tolto xchè basta una sola volta in testata.
095800060118      * ?***  Dopo attivazione CEDINTESA il nuovo traduttore,x ogni dettaglio,
095900060118      * ?***  riportava il TDT+20+3 cosa che stranamente non faceva il vecchio
096000060118      * ?***  traduttore AS400. (segnalato da Lynx il problema)
096100060118     C******
096200960530     C*  Scrittura record D10
096300960705     C                   CLEAR                   WDAT
096400960705     C                   MOVEL     EDTD10        WDAT
096500960726     C                   Z-ADD     §MSNUM        SUMPRG
096600960705     C                   MOVEL     'TD10'        SUMTPR
096700960705     C                   MOVEL     WDAT          SUMDAT
096800960704     C                   WRITE     RRIFCSUM
096900960530     C*
097000960530     C                   ENDSR
097100960530     C*--------------------------------------------------------------*
097200960530     C* WRTD15: scirvo tipo record D15 in IFCSUM                    -*
097300960530     C*--------------------------------------------------------------*
097400960530     C     WRTD15        BEGSR
097500960530     C*
097600031211     C* Prima deve essere scritto il Mittente e poi il destinatario
097700031211     C* questa è una regola altrimenti non funzionano le informazioni
097800031211     C* di riferimento del destinatario/telefono
097900031211     C*
098000031211     C*  Scrivo D15 x mittente
098100031211     C                   CLEAR                   EDTD15
098200031211     C                   MOVEL     'CZ'          DD3035
098300031211     C* ---------
098400031211     C*  Se codice cliente mittente = 0 metto codice cliente spedizione
098500031211     C*   (prima metteva il cod. Part.IVA Europeo)
098600031211     C                   clear                   WIVAM            16
098700031211     C                   MOVEL     ARBCCM        WIVAM            16
098800031211     C     ARBCCM        IFEQ      0
098900031211     C                   MOVEL     ARBKSC        WIVAM
099000031211     C                   END
099100031211     C* ---------
099200031211     C                   MOVEL     WIVAM         DD3039
099300031211     C*  Altri dati destinatario
099400031211     C                   MOVEL     ARBRSM        DD3036
099500031211     C                   MOVEL     ARBINM        DD3042
099600031211     C                   MOVEL     ARBLOM        DD3164
099700031211     C                   MOVEL     ARBCAM        DD3251
099800031211     C                   Z-ADD     1             YY
099900031211     C     ARBNZM        LOOKUP    C15(YY)                                32
100000031211     C   32ISO(YY)       COMP      *BLANKS                            3232
100100031211     C  N32              MOVEL     'IT'          DD3207
100200031211     C   32              MOVEL     ISO(YY)       DD3207
100300031211     C*  Scrittura record D15
100400031211     C                   CLEAR                   WDAT
100500031211     C                   MOVEL     EDTD15        WDAT
100600031211     C                   Z-ADD     §MSNUM        SUMPRG
100700031211     C                   MOVEL     'TD15'        SUMTPR
100800031211     C                   MOVEL     WDAT          SUMDAT
100900031211     C                   WRITE     RRIFCSUM
101000031211     C*
101100960530     C                   CLEAR                   EDTD15
101200960530     C*  Scrivo D15 x destinatario
101300960530     C                   MOVEL     'CN'          DD3035
101400960712     C*  Reperisco P.Iva destinatario
101500960712     C                   MOVEL     'P'           KTRC
101600060213     C     KAR4          CHAIN     fiar401L                           32
101700960712     C     *IN32         IFEQ      '0'
101800961230     C                   MOVE      AR4NOT        WIVAD            16
101900961230     C                   MOVEL     WIVAD         DD3039
102000011031     C                   MOVEL     AR4NOT        WIVAM            16
102100960712     C                   ELSE
102200960730     C                   MOVEL     ARBKSC        DD3039
102300960712     C*  Se codice cliente mittente = 0 metto codice
102400960712     C*  destinatario
102500960712     C                   MOVEL     ARBCCM        WIVAM            16
102600960712     C     ARBCCM        IFEQ      0
102700960730     C                   MOVEL     ARBKSC        WIVAM
102800960712     C                   END
102900960712     C                   END
103000960530     C*  Altri dati destinatario
103100960530     C                   MOVEL     ARBRSD        DD3036
103200960530     C                   MOVEL     ARBIND        DD3042
103300960530     C                   MOVEL     ARBLOD        DD3164
103400080924      * chiodo sul CAP
103500080924      * x l'IRLANDA "EIRE" nel CAP deve essere sostituito con "0000"
103600080924     c                   if        arbcad = 'EIRE '
103700080924     C                   MOVEL     '0000'        DD3251
103800080924     c                   else
103900960530     C                   MOVEL     ARBCAD        DD3251
104000080924     c                   end
104100960822     C                   Z-ADD     1             YY                3 0
104200960822     C     ARBNZD        LOOKUP    C15(YY)                                32
104300960822     C     *IN32         IFEQ      '0'
104400960822     C     D82NAZ        LOOKUP    C15(YY)                                32
104500960822     C                   END
104600960823     C*
104700960823     C   32ISO(YY)       COMP      *BLANKS                            3232
104800960822     C  N32              MOVEL     D82NAZ        DD3207
104900960822     C   32              MOVEL     ISO(YY)       DD3207
105000960530     C*  Reperisco seconda ragione sociale destinatario
105100960530     C                   MOVEL     'D'           KTRC
105200060213     C     KAR4          CHAIN     fiar401L                           32
105300960530     C     *IN32         IFEQ      '0'
105400960530     C                   MOVEL     AR4NOT        DDA036
105500960530     C                   END
105600031210      *
105700031210      *  se presenti riporta i riferimenti referente x la consegna e
105800031210      *   il riferimento telefonico
105900031210     c                   movel     'GEN'         ktrd
106000031210     c                   clear                   DAr5Gen
106100031210     c     kar5          chain     fiar501L
106200031210     c                   if        %Found(fiar501L)
106300031210     c                   Movel     Ar5Uni        DAr5Gen
106400031212      * devono comunque essere scritti insieme: CTA..... COM altrimenti non funziona
106500031212     c                   if        §ar5ref <> *blank or
106600031212     c                             §ar5teld <> *blank
106700031212     C                   MOVEL     'IC'          DD3139
106800031212     C                   MOVEL     'TE'          DD3155
106900031210     c                   if        §ar5ref <> *blank
107000031210     c                   movel     §ar5ref       DD3412
107100031212     c                   else
107200031212     c                   movel(P)  'X'           DD3412
107300031212     c                   end
107400031210     c                   if        §ar5teld <> *blank
107500031210     c                   movel     §ar5teld      DD3148
107600031212     c                   else
107700031212     c                   movel(P)  'X'           DD3148
107800031210     c                   end
107900031210     c                   end
108000031212     c                   end
108100031210      *
108200960530     C*  Scrittura record D15
108300960705     C                   CLEAR                   WDAT
108400960705     C                   MOVEL     EDTD15        WDAT
108500960726     C                   Z-ADD     §MSNUM        SUMPRG
108600960705     C                   MOVEL     'TD15'        SUMTPR
108700960705     C                   MOVEL     WDAT          SUMDAT
108800960704     C                   WRITE     RRIFCSUM
108900960530     C*
109000960530     C                   ENDSR
109100960530     C*--------------------------------------------------------------*
109200960530     C* WRTD20: scirvo tipo record D20 in IFCSUM                    -*
109300960530     C*--------------------------------------------------------------*
109400960530     C     WRTD20        BEGSR
109500960530     C*
109600960530     C                   CLEAR                   EDTD20
109700960530     C*  Scrivo totale colli spedizione
109800960530     C                   Z-ADD     99999         DE1496
109900960530     C                   Z-ADD     ARBNCL        DE7224
110000960807     C                   MOVEL     'CT'          DE7065
110100960530     C*  Addiziono in totale colli
110200960530     C                   ADD       ARBNCL        WTONCL
110300100408     C*  Natura Merce
110400100408     C                   IF        §PUFTXAAA = 'S' and arbNAS <> *blank
110500100408     C                   MOVEL     'AAA'         DE4451
110600100408     C                   MOVEL     arbNAS        DE4440
110700100408     c                   end
110800100408     C*
110900960530     C*  Scrittura record D20
111000960705     C                   CLEAR                   WDAT
111100960705     C                   MOVEL     EDTD20        WDAT
111200960726     C                   Z-ADD     §MSNUM        SUMPRG
111300960705     C                   MOVEL     'TD20'        SUMTPR
111400960705     C                   MOVEL     WDAT          SUMDAT
111500960704     C                   WRITE     RRIFCSUM
111600960530     C*
111700960530     C                   ENDSR
111800960530     C*--------------------------------------------------------------*
111900960530     C* WRTD25: scirvo tipo record D25 in IFCSUM                    -*
112000960530     C*--------------------------------------------------------------*
112100960530     C     WRTD25        BEGSR
112200960530     C*
112300960530     C                   CLEAR                   EDTD25
112400960530     C*  Specifico peso
112500960530     C                   MOVEL     'WT '         DF6311
112600960903     C                   MOVEL     'AAG'         DF6313
112700960530     C                   MOVEL     'KGM'         DF6411
112800150825     C*
112900150825     C*  reperisce il volume/peso da CML o da Bollettazione
113000150825     c                   exsr      VOL_PES
113100150825      ** -------
113200150825      **  SOSTITUITO tutto dal calcolo con la VOL_PES
113300961126     C* Determino peso tassabile per le spedizioni in assegnato
113400961126     C* (è il maggiore fra ARBPKF e il risultato delle moltiplicazione)
113500150825     C***  WPOR          IFEQ      'A'
113600150825     C***  §7AKPM        MULT(H)   ARBVLF        PESPRT            7 1
113700150825     C***  ARBPKF        IFGT      PESPRT
113800150825     C***                Z-ADD     ARBPKF        PESPRT
113900150825     C***                END
114000150825     C***                ELSE
114100961126     C* Per le spedizioni in franco,invece,stampo il peso da fatturare
114200150825     C***                Z-ADD     ARBPKF        PESPRT
114300150825     C***                END
114400150825     C***
114500150825     C***                Z-ADD(H)  PESPRT        WPKF              7 0
114600150825      *
114700150825      **  toglie il decimale
114800150825     C                   Z-ADD(H)  wpes          wpes0             7 0
114900150825     C     wpes0         IFEQ      0
115000150825     C                   Z-ADD     1             wpes0
115100150825     C                   END
115200150825     C                   Z-ADD     wpes0         DF6314
115300150825      *
115400150825     C** Addiziono in totale peso x intero messaggio
115500150825     C                   ADD       wpes0         WTOPKF
115600150825      ** -------
115700960530     C*  Scrittura record D25
115800960705     C                   CLEAR                   WDAT
115900960705     C                   MOVEL     EDTD25        WDAT
116000960726     C                   Z-ADD     §MSNUM        SUMPRG
116100960705     C                   MOVEL     'TD25'        SUMTPR
116200960705     C                   MOVEL     WDAT          SUMDAT
116300960704     C                   WRITE     RRIFCSUM
116400010424     C*
116500960530     C*  Specifico volume
116600010730     C     §PUVOL        IFeq      'S'
116700010424     C*
116800010424     C                   CLEAR                   EDTD25
116900010424     C                   MOVEL     'VOL'         DF6311
117000010424     C                   MOVEL     'MTQ'         DF6411
117100150825      ** -------
117200150825     C***                Z-ADD(H)  ARBVLF        WVLF              4 2
117300150825      *
117400150825     C                   Z-ADD(H)  wvol          wvol0             4 2
117500150825     C     wvol0         IFEQ      *ZEROS
117600150825     C                   Z-ADD     0,01          wvol0
117700150825     C                   END
117800150825     C                   Z-ADD     wvol0         DF6314
117900150825      *
118000150825     C** Addiziono in totale peso x intero messaggio
118100150825     C                   ADD       wvol0         WTOVLF
118200150825      ** -------
118300960530     C*  Scrittura record D25
118400010424     C                   CLEAR                   WDAT
118500010424     C                   MOVEL     EDTD25        WDAT
118600010424     C                   Z-ADD     §MSNUM        SUMPRG
118700010424     C                   MOVEL     'TD25'        SUMTPR
118800010424     C                   MOVEL     WDAT          SUMDAT
118900010424     C                   WRITE     RRIFCSUM
119000010424     C*
119100010424     C                   End
119200960530     C*
119300960530     C                   ENDSR
119400150825     C*****************************************************************
119500150825     C*  Reperisce Peso/Volume o da CML o da Bollettato
119600150825     C*****************************************************************
119700150825     C     VOL_PES       BEGSR
119800150825     C*
119900150825     c     *like         define    arbvlb        wvol
120000150825     c     *like         define    arbpkb        wpes
120100150825     C     *LIKE         DEFINE    §qttpc        WTARA
120200150825      *
120300150825      * Prima cerca il volume fra bollettato e CML
120400150825     C     arbncR        ifeq      arbncl
120500150825      * se è considerata un'unica spedizione
120600150825     C                   z-add     ARBVLC        WVOL
120700150825     C                   else
120800150825     C*
120900150825     C     ARBVLC        ifge      ARBVLb
121000150825     C                   z-add     ARBVLC        WVOL
121100150825     C                   else
121200150825     C                   z-add     ARBVLb        WVOL
121300150825     C                   end
121400150825     C*
121500150825     C                   endif
121600150825      *
121700150825     C* PESO VDL - TARA
121800150825     C     §QTTPC        MULT      arbncp        WTARA
121900150825     C                   Z-ADD     arbPKC        wpes
122000150825     c                   sub(h)    wtara         wpes
122100150825      *
122200150825      * Poi cerca il peso fra bollettato e CML
122300150825     C     arbncP        ifeq      arbncl
122400150825      * se è considerata un'unica spedizione
122500150825     C                   ELSE
122600150825     C*  altrimenti se il bollettato è superiore passo il Bollettato.
122700150825     C     wpes          iflt      arbPKb
122800150825     C                   Z-ADD     arbPKb        wpes
122900150825     C                   end
123000150825     C*
123100150825     C                   ENDIF
123200150825     C*
123300150825     C                   ENDSR
123400960530     C*--------------------------------------------------------------*
123500960530     C* WRTD30: scirvo tipo record D30 in IFCSUM                    -*
123600960530     C*--------------------------------------------------------------*
123700960530     C     WRTD30        BEGSR
123800960530     C*
123900960530     C*  Scrivo dettaglio segnacolli
124000960530     C     KART          CHAIN     FNART01L                           31
124100120330      *
124200960530     C     *IN31         DOWEQ     '0'
124300960530     C                   CLEAR                   EDTD30
124400120330      *
124500120330     C                   Z-ADD     1             X
124600120330      *
124700960530     C*  Scrivo fino a 10 segnacolli o fino a che non è finito dettag.
124800120330     C*****X             DOWLE     20
124900120330     C     X             DOWLE     Quanti_X_PCI
125000960530     C     *IN31         ANDEQ     '0'
125100120330      *
125200120413     C********           ADD       1             X
125300120330      *
125400051017     C*  Se tipo segnacollo 'E'=Euroexpress aggancio FIARS
125500980219     C     D82TSC        IFEQ      'E'
125600980219     C                   MOVEL     'E'           KTRC
125700980219     C                   Z-ADD     ARTFLS        KFLS
125800980219     C                   Z-ADD     ARTLNA        KLNA
125900980219     C                   Z-ADD     ARTNRS        KNRS
126000980219     C                   Z-ADD     ARTNSC        KNSC
126100051017     C     KARS          CHAIN     FIARS01L                           35
126200980219     C     *IN35         IFEQ      '0'
126300980219     C                   MOVEL     ARSNOT        D30(X)
126400980219     C                   END
126500980219     C                   ELSE
126600980219     C                   MOVEL     DSSGN         D30(X)
126700980219     C                   END
126800120413      *
126900120413      * spostato il contatore
127000120413     C                   ADD       1             X
127100960530     C     KART          READE     FNART01L                               31
127200960530     C                   END
127300120330      *
127400120330      *  Se presente un Qualificatore lo imposta
127500120330     c                   if          Qualificatore_PCI <> *blank
127600120330     c                   eval      DG4233 = Qualificatore_PCI
127700120330     c                   end
127800120330      *
127900960530     C*  Scivo record TD30
128000960705     C                   CLEAR                   WDAT
128100960705     C                   MOVEL     EDTD30        WDAT
128200980219     C                   Z-ADD     §MSNUM        SUMPRG
128300960705     C                   MOVEL     'TD30'        SUMTPR
128400960705     C                   MOVEL     WDAT          SUMDAT
128500960704     C                   WRITE     RRIFCSUM
128600120330     C*
128700960530     C                   END
128800960530     C*
128900960530     C                   ENDSR
129000970203     C*--------------------------------------------------------------*
129100970203     C* RIFPAT: Reperisco riferimento originale partner             -*
129200970203     C*--------------------------------------------------------------*
129300970203     C     RIFPAT        BEGSR
129400970203     C*
129500970203     C                   MOVEL     AR4NOT        TESTO1            5
129600020711     c                   clear                   lnpnot            4
129700020711     C                   eval      LNPnot = %subst(ar4not:21:4)
129800970203     C                   SETON                                        34
129900970203     C*  Se la nota partenza mi indica che il reso è stato effettuato
130000970203     C*  da giacenza mi posiziono si giac. e reperisco nr.spedizione
130100020711     C                   If        TESTO1 = COST2 and LNPnot='LNP.'
130200970203     C                   MOVEL     AR4NOT        RIFGIA
130300970203     C                   Z-ADD     GCAAGC        KAGC
130400970203     C                   Z-ADD     GCAFGC        KFGC
130500970203     C                   Z-ADD     GCANGC        KNGC
130600970203     C                   Z-ADD     0             KFRG
130700970203     C     KAGC          IFGT      60
130800970203     C                   ADD       1900          KAGC
130900970203     C                   ELSE
131000970203     C                   ADD       2000          KAGC
131100970203     C                   END
131200050309     C     KGCP          CHAIN     TIGCP02L                           31
131300970203     C                   Z-ADD     GCPAAS        KAAP
131400970203     C                   Z-ADD     GCPLNP        KLPP
131500970203     C                   Z-ADD     GCPNRS        KNRP
131600970203     C                   Z-ADD     GCPNSP        KNPP
131700060213     C* Mi posiziono su ar4 x aggangiare rif.partner
131800970203     C                   MOVEL     'E'           KTRC
131900060213     C     KBL4          CHAIN     Fiar401L                           34
132000970203     C                   ELSE
132100970203     C                   MOVEL     AR4NOT        TESTO3
132200001222     C*
132300970203     C*  altrimenti verifico se la nota è di reso fuori da giacenza
132400970203     C*  reperisco nr.spedizione da essa
132500970203     C     TESTO3        IFEQ      COST1
132600001222      *
132700001222     C                   MOVEL     'E'           KTRC
132800001222     C                   MOVEL     AR4NOT        dgtSPE
132900001222      *
133000001222     C*   controlla che ci siano solo numerici nel campo di testo
133100001222      *    altrimenti è come se non fosse riuscita la CHAIN seguente.
133200001222     C     digits        check     chkspe:1
133300001222      *
133400001222     C                   IF        %Found
133500001222     C                   seton                                        34
133600001222     C                   ELSE
133700001222     C*
133800001222     C                   MOVEL     AR4NOT        RIFSPE
133900970203     C                   Z-ADD     RAAS          KAAP
134000970203     C                   Z-ADD     RLNP          KLPP
134100970203     C                   Z-ADD     RNRS          KNRP
134200970203     C                   Z-ADD     RNSP          KNPP
134300970203     C     KAAP          IFGT      60
134400970203     C                   ADD       1900          KAAP
134500970203     C                   ELSE
134600970203     C                   ADD       2000          KAAP
134700970203     C                   END
134800001222     C*
134900060213     C* Mi posiziono su ar4 x aggangiare rif.partner
135000060213     C     KBL4          CHAIN     Fiar401L                           34
135100001222     C                   end
135200001222     C*
135300060213     C*  Se non trovato mi posiziono su ar4 con la  sped.originale
135400970203     C     *IN34         IFEQ      '1'
135500970203     C     KLBL          CHAIN     FNLBL02L                           31
135600970203     C     *IN31         IFEQ      '0'
135700970203     C                   Z-ADD     LBLAAO        KAAP
135800970203     C                   Z-ADD     LBLLPO        KLPP
135900970203     C                   Z-ADD     LBLNRO        KNRP
136000970203     C                   Z-ADD     LBLNSO        KNPP
136100970203     C                   MOVEL     'E'           KTRC
136200060213     C     KBL4          CHAIN     Fiar401L                           34
136300970203     C                   END                                                    trv.lbl
136400970203     C                   END                                                    trv bl4
136500970203     C*
136600970203     C                   END                                                    = testo3
136700970203     C                   END                                                    = testo1
136800970203     C*  Se trovato rif.partner originale lo scrivo nelle note
136900970203     C     *IN34         IFEQ      '0'
137000060213     C                   MOVEL     ar4NOT        WCOM15           15
137100970203     C                   MOVEL     D05(X)        WCOM50           50
137200970203     C                   MOVE      WCOM15        WCOM50
137300970203     C                   MOVEL     WCOM50        D05(X)
137400970203     C                   END
137500970203     C*
137600970203     C                   ENDSR
137700950428     C*****************************************************************
137800950428     C* OPERAZIONI INIZIALI
137900950428     C*****************************************************************
138000950428     C     *INZSR        BEGSR
138100010426     C*
138200960530     C* Definisco chiavi di accesso
138300950428     C     KARB          KLIST
138400960530     C                   KFLD                    KAAS
138500960530     C                   KFLD                    KLNP
138600960530     C                   KFLD                    KNRS
138700960530     C                   KFLD                    KNSP
138800960530     C     KART          KLIST
138900960530     C                   KFLD                    KAAS
139000960530     C                   KFLD                    KLNP
139100960530     C                   KFLD                    KNRS
139200960530     C                   KFLD                    KNSP
139300980219     C     KARS          KLIST
139400980219     C                   KFLD                    KFLS
139500980219     C                   KFLD                    KLNA
139600980219     C                   KFLD                    KNRS
139700980219     C                   KFLD                    KNSC
139800980219     C                   KFLD                    KTRC
139900970203     C     KLBL          KLIST
140000970203     C                   KFLD                    KAAS
140100970203     C                   KFLD                    KLNP
140200970203     C                   KFLD                    KNRS
140300970203     C                   KFLD                    KNSP
140400970203     C     KGCP          KLIST
140500970203     C                   KFLD                    KAGC
140600970203     C                   KFLD                    KFGC
140700970203     C                   KFLD                    KNGC
140800970203     C                   KFLD                    KFRG
140900970203     C     KBL4          KLIST
141000970203     C                   KFLD                    KAAP
141100970203     C                   KFLD                    KLPP
141200970203     C                   KFLD                    KNRP
141300970203     C                   KFLD                    KNPP
141400970203     C                   KFLD                    KTRC
141500950428     C     KAR4          KLIST
141600960530     C                   KFLD                    KAAS
141700960530     C                   KFLD                    KLNP
141800960530     C                   KFLD                    KNRS
141900960530     C                   KFLD                    KNSP
142000960530     C                   KFLD                    KTRC
142100031210     C     KAR5          KLIST
142200031210     C                   KFLD                    KAAS
142300031210     C                   KFLD                    KLNP
142400031210     C                   KFLD                    KNRS
142500031210     C                   KFLD                    KNSP
142600031210     C                   KFLD                    KTRD              3
142700090223     C     KAR6          KLIST
142800090223     C                   KFLD                    KAAS
142900090223     C                   KFLD                    KLNP
143000090223     C                   KFLD                    KNRS
143100090223     C                   KFLD                    KNSP
143200960530     C     KAR9          KLIST
143300960530     C                   KFLD                    KAAS
143400960530     C                   KFLD                    KLNP
143500960530     C                   KFLD                    KNRS
143600960530     C                   KFLD                    KNSP
143700960530     C     KTAB          KLIST
143800960530     C                   KFLD                    KKUT
143900960530     C                   KFLD                    KCOD
144000961126     C     KTAB1         KLIST
144100961126     C                   KFLD                    KKUT
144200961126     C                   KFLD                    KCOD
144300961126     C                   KFLD                    KKEY
144400960726     C     KTABE         KLIST
144500960726     C                   KFLD                    KCOD1
144600960726     C                   KFLD                    KKEY1
144700120119     C     Korm          KLIST
144800120119     C                   KFLD                    ORMPOE
144900120119     C                   KFLD                    ORMNSR
145000120119     C                   KFLD                    ORMNOR
145100120119     C                   KFLD                    ORMNRV
145200171018     C     Kpnd          KLIST
145300171018     C                   KFLD                    KAAS
145400171018     C                   KFLD                    KLNP
145500171018     C                   KFLD                    KNRS
145600171018     C                   KFLD                    KNSP
145700960530     C*  Definisco variabili
145800960726     C     *LIKE         DEFINE    TABKEY        KKEY1
145900960530     C     *LIKE         DEFINE    ARBAAS        KAAS
146000960530     C     *LIKE         DEFINE    ARBLNP        KLNP
146100960530     C     *LIKE         DEFINE    ARBNRS        KNRS
146200960530     C     *LIKE         DEFINE    ARBNSP        KNSP
146300960530     C     *LIKE         DEFINE    AR4TRC        KTRC
146400960530     C     *LIKE         DEFINE    TABCOD        KCOD1
146500960530     C     *LIKE         DEFINE    TBLKUT        KKUT
146600960530     C     *LIKE         DEFINE    TBLCOD        KCOD
146700961126     C     *LIKE         DEFINE    TBLKEY        KKEY
146800970203     C     *LIKE         DEFINE    GCPAGC        KAGC
146900970203     C     *LIKE         DEFINE    GCPFGC        KFGC
147000970203     C     *LIKE         DEFINE    GCPNGC        KNGC
147100970203     C     *LIKE         DEFINE    GCPFRG        KFRG
147200060213     C     *LIKE         DEFINE    ar4AAS        KAAP
147300060213     C     *LIKE         DEFINE    ar4LNP        KLPP
147400060213     C     *LIKE         DEFINE    ar4NRS        KNRP
147500060213     C     *LIKE         DEFINE    ar4NSP        KNPP
147600980219     C     *LIKE         DEFINE    ARSFLS        KFLS
147700980219     C     *LIKE         DEFINE    ARSLNA        KLNA
147800980219     C     *LIKE         DEFINE    ARSNSC        KNSC
147900970203     C* InizializzoIvariabil FtotaleOD    KCOD1
148000960530     C                   Z-ADD     0             WPROG             5 0
148100960530     C                   Z-ADD     0             WTONCL            9 0
148200961016     C                   Z-ADD     0             WTOPKF           10 1
148300961016     C                   Z-ADD     0             WTOVLF           12 3
148400960530     C* Imposto data del giorno
148500960530     C                   TIME                    W0140            14 0
148600960530     C                   MOVE      W0140         G02DAT            8 0
148700960530     C                   MOVEL     *BLANK        G02ERR
148800960530     C                   CALL      'XSRDA8'
148900960530     C                   PARM                    WLBDA8
149000960530     C                   MOVEL     G02INV        WOGGI             8 0
149100010424      *------------
149200010424     C* Caricamento Tabella Partner esteri
149300010424     C                   Z-ADD     0             X                 3 0
149400010424     C                   MOVEL     'PT'          TABCOD
149500010424     C     TABCOD        CHAIN     EDTAB01L                           30
149600010424     C     *IN30         DOWEQ     '0'
149700010424     C     TABFLG        IFEQ      *BLANKS
149800010424     C                   ADD       1             X
149900010424     C                   MOVEL     TABKEY        CPT(X)
150000010424     C                   MOVEL     TABUNI        DPT(X)
150100010424     C                   MOVEL     TABUNI        KSC(X)
150200010424     C                   END
150300010424     C     TABCOD        READE     EDTAB01L                               30
150400010424     C                   END
150500121105      *
150600121105      *  se deve inviare la mail di alert x limite quasi raggiunto.
150700121105     c                   exsr      ChecK_PT
150800010730      *------------
150900010730     C* Caricamento Tabella Partner esteri aggiuntiva
151000010730     C                   MOVEL     'PU'          TABCOD
151100010730     C     TABCOD        CHAIN     EDTAB01L                           30
151200010730     C     *IN30         DOWEQ     '0'
151300010730     C     TABFLG        IFEQ      *BLANKS
151400010730     C                   MOVEL     TABKEY        CODPU            35
151500010730     C                   Z-ADD     1             X
151600010730     C     CODPU         LOOKUP    CPT(X)                                 32
151700010730     C   32              MOVEL     TABUNI        DPU(X)
151800010730     C                   END
151900010730     C     TABCOD        READE     EDTAB01L                               30
152000010730     C                   END
152100120119      *------------
152200120119     C* Caricamento Tabella Partner esteri aggiuntiva PV
152300120119     C                   MOVEL     'PV'          TABCOD
152400120119     C     TABCOD        CHAIN     EDTAB01L                           30
152500120119     C     *IN30         DOWEQ     '0'
152600120119     C     TABFLG        IFEQ      *BLANKS
152700120119     C                   MOVEL     TABKEY        CODPV            35
152800120119     C                   Z-ADD     1             X
152900120119     C     CODPV         LOOKUP    CPT(X)                                 32
153000120119     C   32              MOVEL     TABUNI        DPV(X)
153100120119     C                   END
153200120119     C     TABCOD        READE     EDTAB01L                               30
153300120119     C                   END
153400120705      *------------
153500120705     C* Caricamento Tabella Partner esteri aggiuntiva PZ
153600120705     C                   MOVEL     'PZ'          TABCOD
153700120705     C     TABCOD        CHAIN     EDTAB01L                           30
153800120705     C     *IN30         DOWEQ     '0'
153900120705     C     TABFLG        IFEQ      *BLANKS
154000120705     C                   MOVEL     TABKEY        CODPZ            35
154100120705     C                   Z-ADD     1             X
154200120705     C     CODPZ         LOOKUP    CPT(X)                                 32
154300120705     C   32              MOVEL     TABUNI        DPZ(X)
154400120705     C                   END
154500120705     C     TABCOD        READE     EDTAB01L                               30
154600120705     C                   END
154700170412      *------------
154800170412     C* Caricamento Tabella Partner esteri aggiuntiva PS (invio Alert)
154900170412     C                   MOVEL     'PS'          TABCOD
155000170412     C     TABCOD        CHAIN     EDTAB01L                           30
155100170412     C     *IN30         DOWEQ     '0'
155200170412     C     TABFLG        IFEQ      *BLANKS
155300170412     C                   MOVEL     TABKEY        CODPS            35
155400170412     C                   Z-ADD     1             X
155500170412     C     CODPS         LOOKUP    CPT(X)                                 32
155600170412     C   32              MOVEL     TABUNI        DPs(X)
155700170412     C                   END
155800170412     C     TABCOD        READE     EDTAB01L                               30
155900170412     C                   END
156000010424      *------------
156100960530     C* Caricamento Tabella Priorità trasporto
156200140428      *   LA TABELLA è COMUNQUE IGNORATA poichè utilizzata solo in IMPORT
156300140428      *   qui è stato cambiato il modo di inviare l'informazione del Tipo Servizio.
156400140428      *   Viene mandato solo in particolare situazione x indicare l'appuntamento oppure
156500140428      *   il priority o altro.
156600960530     C                   Z-ADD     0             X                 3 0
156700960530     C                   MOVEL     'TS'          KCOD1
156800960530     C     KCOD1         CHAIN     EDTAB01L                           30
156900960530     C     *IN30         DOWEQ     '0'
157000960530     C     TABFLG        IFEQ      *BLANKS
157100070220     c                   move      tabkey        due_alfa          2
157200070220     c                   if        due_alfa = *blank
157300960530     C                   ADD       1             X
157400960530     C                   MOVEL     TABKEY        PTR(X)
157500960530     C                   MOVEL     TABUNI        TSP(X)
157600070220     c                   end
157700960530     C                   END
157800960530     C     KCOD1         READE     EDTAB01L                               30
157900960530     C                   END
158000960716     C* Caricamento Tabella Tipo Incasso C/Assegno
158100960716     C                   Z-ADD     0             X                 3 0
158200960716     C                   MOVEL     '1A'          TABCOD
158300960716     C     TABCOD        CHAIN     EDTAB01L                           30
158400960716     C     *IN30         DOWEQ     '0'
158500960716     C     TABFLG        IFEQ      *BLANKS
158600960716     C                   ADD       1             X
158700960716     C                   MOVEL     TABKEY        TPI(X)
158800130215     C                   MOVEL     TABKEY        TPIkey(X)
158900960716     C                   MOVEL     TABUNI        C1A(X)
159000960716     C                   END
159100960716     C     TABCOD        READE     EDTAB01L                               30
159200960716     C                   END
159300960530     C* Caricamento Tabella termini consegna
159400960530     C                   Z-ADD     0             X                 3 0
159500960530     C                   MOVEL     'TB'          KCOD1
159600960530     C     KCOD1         CHAIN     EDTAB01L                           30
159700960530     C     *IN30         DOWEQ     '0'
159800960530     C     TABFLG        IFEQ      *BLANKS
159900070220     c                   move      tabkey        due_alfa          2
160000140428      ** carica tutto anche i fuori STD x particolarità legate al PTN da inviare
160100140428     c***********        if        due_alfa = *blank
160200960530     C                   ADD       1             X
160300960530     C                   MOVEL     TABKEY        TCO(X)
160400960530     C                   MOVEL     TABUNI        CTB(X)
160500140428      * x gestire dei codici particolari al posto di quelli standard devo memorizzare
160600140428      * il codice del tipo bolla (1 byte) + l'eventuale suffisso (2 bytes)
160700140428      * nel codice di 3 bytes ho la chiave da comparare a programma + blank(std) o
160800140428      * l'eventuale suffisso x il Partner da gestire in modo differente.
160900140428     C                   eval       CTBkey(X) = %subst(tabUNI:1:1) + due_alfa
161000140428     c***********        end
161100960530     C                   END
161200960530     C     KCOD1         READE     EDTAB01L                               30
161300960530     C                   END
161400960726     C* Caricamento NUMERATORE
161500000327     C                   clear                   X                 3 0
161600960726     C                   Z-ADD     1             §MSNUM
161700960726     C                   MOVEL     'MS'          KCOD1
161800000327     C                   MOVEL(P)  '785'         KKEY1
161900000327     C     KTABE         CHAIN     EDTAB01L
162000000327     C                   if        %found and TABFLG = *BLANKS
162100960726     C                   MOVEL     TABUNI        DSMS
162200960726     C                   ADD       1             §MSNUM
162300061215      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
162400061215      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
162500061215      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
162600061215      *  Attenzione per gestire Pracht che suddivide il messaggio in 4 occorre prevedere
162700061215      *  un salto di numerazione per evitare problemi.
162800061215     c                   move      §MSNUM        campo_4n          4 0
162900061215     c                   if        campo_4n >= 9996
163000061215     c                   add       5             §MSNUM
163100061215     c                   end
163200960726     C                   Z-ADD     WOGGI         §MSDAT
163300960726     C                   MOVEL     DSMS          TABUNI
163400960726     C                   UPDATE    EDTAB000
163500960726     C                   END
163600000327      * Caricamento Tabella codici bolla
163700000327     C                   clear                   X
163800000327     C                   MOVEL     'FF'          KCOD1
163900000327     C     KCOD1         setll     EDTAB000
164000000327     C     KCOD1         reade     EDTAB000
164100000327     C                   DOW       not %eof
164200010220     C                   If         TabFLG = *BLANKS
164300000327     C                   ADD       1             X
164400010220     C                   MOVEL     TabKEY        CFF(X)
164500010220     C                   MOVEL     TabUNI        DSFF
164600000327     C                   MOVEL     §FFIDN        DFF(X)
164700000327     C                   ENDif
164800000327     C     KCOD1         reade     EDTAB000
164900000327     C                   ENDDO
165000960530     C* Caricamento Tabella codici bolla
165100960530     C                   Z-ADD     0             X                 3 0
165200960530     C                   MOVEL     '3A'          KCOD
165300960530     C                   Z-ADD     1             KKUT
165400960530     C     KTAB          CHAIN     TABEL00F                           30
165500960530     C     *IN30         DOWEQ     '0'
165600960530     C     TBLFLG        IFEQ      *BLANKS
165700960530     C                   ADD       1             X
165800960530     C                   MOVEL     TBLKEY        C3A(X)
165900960530     C                   MOVEL     TBLUNI        D3A(X)
166000960530     C                   END
166100960530     C     KTAB          READE     TABEL00F                               30
166200960530     C                   END
166300960822     C* Caricamento Tabella codici nazioni
166400960822     C                   Z-ADD     0             X                 3 0
166500960822     C                   MOVEL     '15'          KCOD
166600960822     C                   Z-ADD     1             KKUT
166700960822     C     KTAB          CHAIN     TABEL00F                           30
166800960822     C     *IN30         DOWEQ     '0'
166900960822     C     TBLFLG        IFEQ      *BLANKS
167000960822     C                   ADD       1             X
167100960822     C                   MOVEL     TBLKEY        C15(X)
167200960822     C                   MOVEL     TBLUNI        DS15
167300960822     C                   MOVEL     §15COD        ISO(X)
167400170418     C                   MOVEL     §15PREF       prf(X)
167500960822     C                   END
167600960822     C     KTAB          READE     TABEL00F                               30
167700960822     C                   END
167800960530     C* Caricamento Tabella CV
167900960530     C                   MOVEL     'CV'          KCOD
168000960530     C     KTAB          CHAIN     TABEL00F                           30
168100960530     C     *IN30         DOWEQ     '0'
168200960530     C     TBLFLG        IFEQ      *BLANKS
168300960530     C                   MOVEL     TBLUNI        DSCV
168400960530     C     §CVITA        IFEQ      'I'
168500960530     C                   MOVEL     TBLKEY        WDIVIT            3
168600960530     C                   END
168700960530     C                   END
168800960530     C     KTAB          READE     TABEL00F                               30
168900960530     C                   END
169000961126     C* Reperisco Kg per metro cubo per calcolo peso tassabile
169100961126     C                   MOVEL     '7A'          KCOD
169200961126     C                   MOVEL(P)  '2'           KKEY
169300961126     C     KTAB1         CHAIN     TABEL00F                           31
169400961126     C  N31              MOVEL     TBLUNI        DS7A2
169500961126     C   31              CLEAR                   DS7A2
169600150825     C*
169700150825     C** CARICAMENTO DATI VARI TASSAZIONE
169800150825     C                   Z-ADD     1             KKUT
169900150825     C                   MOVEL     'QT'          kcod
170000150825     C                   MOVEL(P)  '1'           kKEY
170100150825     C     KTAB1         CHAIN     TABEL00F                           31
170200150825     C  N31              MOVEL     TBLUNI        DSQT1
170300150825     C   31              CLEAR                   DSQT1
170400150825     C*
170500150825      *------------
170600960530     C* finta read x IFCSUM
170700960530     C   01
170800960530     CANN01              READ      EDIFCSUM                               01
170900970124     C*
171000970204     C***                  Z-ADD56        LUNG
171100970204     C***                  MOVELCMD,1     COMMAN
171200970204     C***                  CALL 'QCMDEXC'              90
171300970204     C***                  PARM           COMMAN 80
171400970204     C***                  PARM           LUNG   155
171500970204     C***                  OPEN FNARB01L
171600010424     C*
171700960530     C*
171800960530     C                   ENDSR
171900121105      *---------------------------------------------------------------*
172000121105      * CTRL caricamento schiere    PT                               -*
172100121105      *---------------------------------------------------------------*
172200121105     C     Check_PT      begsr
172300121105     C*
172400121105     c* Controllo riempimento schiera
172500121105     c                   clear                   trul0sds
172600121105     c                   eval      i0sski='CPT'
172700121105     c                   eval      i0sele=%elem(CPT)
172800121105     c                   eval      i0spie=x
172900121105     c                   eval      i0sfile='EDTAB00F'
173000121105     c                   eval      i0ssif=knsif
173100121105     c                   eval      i0spgm='TRTC83R2'
173200121112     c                   movel     kpjbu         kpjbusav
173300121112     c                   clear                   kpjbu
173400121105     c                   movel     trul0sds      kpjbu
173500121105     c                   call      'TRUL0SR'
173600121105     c                   parm                    kpjba
173700121112     c                   movel     kpjbusav      kpjbu
173800121105     C*
173900121105     C                   ENDSR
174000170418     C*****************************************************************
174100170418     C*  Compatta il campo numero di telefono avvicinando i numeri
174200170418     C*****************************************************************
174300170418     C     compatta_NR   BEGSR
174400170418      *
174500170418     c                   z-add     0             in                3 0
174600170418     c                   clear                   Byte_uno          1
174700170418     c                   clear                   out16            16
174800170418      *
174900170418      *  controlla presenza di caratteri divisori o blank
175000170418      *   li elimina e deve compattare il numero
175100170418     c                   do        16            Nx                3 0
175200170418     c                   eval      Byte_uno = %subst(input16:Nx:1)
175300170418      * controlla che NON sia un numero :
175400170418     c                   move      'N'           alfa              1
175500170418     c     digits        check     Byte_uno
175600170418     c                   if        %found
175700170418     c                   move      'S'           alfa
175800170418     c                   end
175900170418      *
176000170418      * tranne sul primo che può esserci il '+'
176100170418     c                   if        Nx = 1 and Byte_uno ='+'
176200170418     c                             or alfa = 'N'
176300170418     c                   add       1             in
176400170418     c                   eval      %subst(out16:in:1) = %subst(input16:Nx:1)
176500170418     c                   end
176600170418     c                   enddo
176700170418      *
176800170418     c                   endsr
176900170418     C*****************************************************************
177000960530     OFNARB000  E            AGGTRA
177100960530     O                       ARBFT3
177200960530     O                       ARBDT3
