000100960530      ***********************************************************************
000200180307      *   ESPORTA DATI BOLLE x FEDEX UK      ESTERO       Export            *
000300960530      ***********************************************************************
000400891004     H DECEDIT('0,') DATEDIT(*DMY/)
000500000207      *---------------------------------------------------------------------*
000600970204     FFNARB01L  UF   E           K DISK    COMMIT
000700020226     FFNART01L  IF   E           K DISK
000800051017     FFIARS01L  IF   E           K DISK
000900060213     Ffiar401L  IF   E           K DISK
001000031210     Ffiar501L  IF   E           K DISK
001100960530     FTABEL00F  IF   E           K DISK
001200180307      *
001300180312     FEDIFCSUM  iF A E             DISK
001400000207      *---------------------------------------------------------------------*
001500000207      * Schiere
001600000207      *---------------------------------------------------------------------*
001700110512     D dvpobrt       e DS
001800020220     D Num             S              1    DIM(11)
001900000207      *---------------------------------------------------------------------*
002000000207      * DS
002100000207      *---------------------------------------------------------------------*
002200010424      *  Ds per reperimento tabella PT e campo risultato
002300020219     D Dati            S           1000
002400180307     D Dati_Bolla      S           1000
002500000207      *---------------------------------------------------------------------*
002600031210     D dar5gen       e ds
002700030521     D DSQT1         E DS                  INZ
002800180312     D TRTC82DS      E DS
002900950428     D KPJBA         E DS
003000960530     D WLBDA8          DS                  INZ
003100950428     D  G02DAT                 1      8  0
003200950428     D  G02INV                 9     16  0
003300950428     D  G02ERR                17     17
003400950428     D  G02TGI                18     22  0
003500970203     D*  Definisco costanti per bolle reso
003600001222     D digits          C                   CONST('0123456789')
003700020222     C***********************************************************************
003800020222     C* Campi del Messaggio impostati Fissi -> eventualmente da modificare <-
003900020222     C***********************************************************************
004000180307     D Virgola         C                   CONST(',')
004100180307     D PuntoDec        C                   CONST('.')
004200180312     C***********************************************************************
004300180312     IRRIFCSUM
004400180312     I              RRIFCSUM                    SUMDAT
004500180312     C***********************************************************************
004600020222     C*---------------------------------------------------------------------*
004700960530     C* Ciclo principale                                                   -*
004800960530     C*---------------------------------------------------------------------*
004900010426     C     *ENTRY        PLIST
005000010426     C                   PARM                    KPJBA
005100180312     C                   MOVEL     KPJBU         TRTC82DS
005200180308     C* scrittura  testata
005300180308     c                   if        testata = *blank
005400180312     c                   eval      dati     = 'Recipient Name,Recipient Company-
005500180312     c                             ,Recipient Address 1,Recipient Address 2,Rec-
005600180308     c                             ipient Post Town,Recipient Post Code,Recipie-
005700180308     c                             nt Telephone,Items,Weight,Bartolini Referenc-
005800180308     c                             e,Bartolini Barcode'
005900180308     c
006000180312     c                   eval      sumDAT = Dati
006100180312     C                   WRITE     RRIFCSUM
006200180312      *
006300180308     c                   move      'S'           TESTATA           1
006400180308     c                   end
006500180307     C*----
006600960530     C*  Se c'è numero spedizione scrivo dettaglio
006700180312    2C     D82NSP        IFNE      0
006800180307     C*----
006900960530     C*  Eseguo posizionamento su archivio bolle per passaggio param.
007000180312     C                   Z-ADD     D82AAS        KAAS
007100180312     C                   Z-ADD     D82LNP        KLNP
007200180312     C                   Z-ADD     D82NRS        KNRS
007300180312     C                   Z-ADD     D82NSP        KNSP
007400020220      * B O L L A
007500960530     C     KARB          CHAIN     FNARB01L                           31
007600020219      *
007700970124    3C     *IN31         IFEQ      *OFF
007800180308      * ------------
007900020220     C* COMPOSIZIONE record BOLLA
008000020219     C* dati bolla
008100180307     C                   clear                   Dati
008200180307     C                   clear                   Dati_Bolla
008300180308     c                   exsr      dati_SPEDIZ
008400180308     c                   exsr      dati_COLLO
008500970124    3C                   END
008600020219     C*
008700960530     C*  Fleggo FNARB come trasmesso
008800180312     c                   if        D82DFV >0
008900960530     C                   MOVEL     'T'           ARBFT3
009000180312     C                   Z-ADD     D82DFV        ARBDT3
009100020227     c                   end
009200020227     C*
009300020227     C                   update    fnarb000
009400970124     C                   SETON                                        RT
009500970124     C*
009600970124   X2C                   ELSE
009700020219     C*
009800970124     C                   SETON                                        LR
009900020227     C*
010000970124    2C                   END
010100960530     C*--------------------------------------------------------------*
010200020219     C* dettaglio spedizione
010300960530     C*--------------------------------------------------------------*
010400180308     C     Dati_SPEDIZ   BEGSR
010500180308     C*
010600180308     c                   move      arbaas        fldaas            4
010700180308     c                   move      arblnp        fldlnp            3
010800180308     c                   move      arbnrs        fldnrs            2
010900180308     c                   move      arbnsp        fldnsp            7
011000180308     C*
011100020219     C* Concatena tutte le informazioni da inserire nel record
011200180307      *       FEDEX vuole queste informazioni separate da virgola poichè è un CSV
011300180307      * .         Recipient Contact Name
011400180307      * ·         Recipient Company Name
011500180307      * ·         Recipient Address Line 1
011600180307      * ·         Recipient Address Line 2
011700180307      * ·         Recipient Post Town
011800180307      * ·         Recipient Post Code
011900180307      * ·         Recipient Telephone (0 if not known)
012000180307      * ·         Items (always 1)
012100180308      * ·--         Weight
012200180308      * ·--         Reference (your reference)
012300180308      * ·--         Barcode content (this must be an exact match
012400180308      *               between the content of the unique barcode on the box,
012500180308      *                 as it presumably must be today)
012600180307      *--------
012700180307      * Nome Destinatario
012800180307     c     ',':' '       xlate     arbRSD        fldRSD
012900180307     c                   eval      Dati =  %Trim(fldRSD)
013000180307      **---
013100180307     C*  Reperisco seconda ragione sociale destinatario
013200180307     C*    attenzione x Bartolini la rag.soc. è su 2 righe x tot.70 chars.
013300180307     C*     ed il secondo pezzo è su fiar4.......
013400180307     C*   per identificare correttamente il nominativo
013500180307     C*  del destinatario straniero.
013600180307     C* Si può effettuare un concatenamento delle informazioni.
013700180307     C                   MOVEL     'D'           KTRC
013800180307     c                   clear                   fldNOT          256
013900180307     c                   clear                   alfNOT
014000180307     C     KAR4          CHAIN     fiar401L
014100180307     c                   if        %Found(fiar401L)
014200180307     c     ',':' '       xlate     ar4not        alfNOT
014300180307     c                   eval      fldnot =%Trim(alfnot)
014400180307     c                   end
014500180307      *
014600180307      *  ma se c'è il riferimento destinatario del FIAR5 prende anche
014700180307      *   questo e lo accoda al dato presente su AR4 cercando al meglio
014800180307      *    di dare il più possibile l'informazione.
014900180307     c                   clear                   DAr5Gen
015000180307     c                   clear                   fldTEL
015100180307     c                   movel     'GEN'         ktrd
015200180307     c     kar5          chain     fiar501L
015300180307     c                   if        %Found(fiar501L)
015400180307     c                   Movel     Ar5Uni        DAr5Gen
015500180307      *--  Telefono referente di consegna
015600180307     c                   if         §ar5telD <> *blank
015700180307     c                   eval      fldTEL = %Trim(§ar5telD)
015800180307     c                   end
015900180307      *--  Referente di consegna
016000180307     c                   if         §ar5ref <> *blank
016100180307     c                   if          fldNOT <> *blank
016200180307     c                   eval      fldnot = %Trim(fldnot) + %Trim('-') +
016300180307     c                             %Trim(§ar5ref)
016400180307     c                   else
016500180307     c                   eval      fldnot = %Trim(§ar5ref)
016600180307     c                   end
016700180307     c                   end
016800180307      *--
016900180307     c                   end
017000180307      *--
017100180307      * Imposta comunque se l'ha trovato un riferimento destinatario
017200180307     c                   if        fldnot <> *blank
017300180307     c                   eval      Dati = %Trim(dati) + '-' +
017400180307     c                                    %Trim(fldnot)
017500180307     c                   end
017600180307      *-
017700180307     c                   eval      Dati = %Trim(dati) + virgola
017800180308      *-
017900180307     C* FINE RAGIONE SOC ------------   Recipient Contact Name
018000180307      *-
018100180308      * Company name
018200180307     c                   eval      Dati = %Trim(dati) + virgola
018300180307     C* FINE COMPANY NAME -----------   Recipient Company Name
018400180307      *--
018500180307     C* Indirizzo Destinatario
018600180307     c     ',':' '       xlate     arbind        fldind
018700180307     c                   eval      Dati = %Trim(dati) +
018800180307     c                                    %Trim(fldind) + virgola
018900180307     C* FINE INDIRIZZO 1 ------------   Recipient Address Line 1
019000180307      *-
019100180307     c                   eval      Dati = %Trim(dati) + virgola
019200180307     C*    E INDIRIZZO 2 ------------   Recipient Address Line 2
019300180307      *
019400180307      * Città
019500180307     c     ',':' '       xlate     arbLOD        fldLOD
019600180307     c                   eval      Dati = %Trim(dati) +
019700180307     c                                    %Trim(fldLOD) + virgola
019800180307      *
019900180307     C* FINE LOCALITA'   ------------   Recipient Post Town
020000180307      *
020100180307      * Zip Code
020200180307     c                   eval      Dati = %Trim(dati) +
020300180307     c                                    %Trim(arbCAD) + virgola
020400180307     C* FINE CAP         ------------   Recipient Post Code
020500180307      *
020600180307      * Telefono del Destinatario
020700180307     c                   if        fldTEL = *blank
020800180307     c                   eval      Dati = %Trim(dati) +
020900180307     c                                    '0' + virgola
021000180307     c                   else
021100180307     c                   eval      Dati = %Trim(dati) +
021200180307     c                                    %Trim(fldTEL) + virgola
021300180307     c                   end
021400180307     C* FINE TELEFONO    ------------   Recipient Telephone (0 if not known)
021500180307      *
021600180307      * Item è sempre =1
021700180307     c                   eval      Dati = %Trim(dati) +
021800180307     c                                    '1' + virgola
021900180307     C* FINE ITEM        ------------   Items (always 1)
022000180307      *
022100180308      *   Salva la parte di dati comuni da ripetere per ogni collo
022200180308     c                   eval        Dati_Bolla = Dati
022300180308      ***
022400180308     c                   endSR
022500180308      * ------------------------------------------------------------------- *
022600180308     C*   Dati singolo collo della spedizione (Tante righe per ogni collo)
022700180308      * ------------------------------------------------------------------- *
022800180308     c     Dati_COLLO    begSR
022900180308      *--
023000180308     C     KARB          setll     FNART01L
023100180308     C     KARB          reade     FNART01L
023200180308      *-
023300180308     c                   dow       not %EoF(FNART01L)
023400180308      *                 *-----------------------*
023500180308     C                   Z-ADD(H)  artPUC        WPKF             12 1
023600180308      *                 *-----------------------*
023700180308     C*  segnacollo
023800180308     C                   move      'E'           KTRC
023900180308     C     KARS          chain     fIARS01L
024000180308     c                   if        %found(fIARS01L)
024100180308      *-
024200180308     C*  prende il volume/peso da CML o da Bollettazione dividendo per i colli
024300180308      *  della spedizione
024400180308      *   (il Peso deve essere quello reale preso o da CML o da Bollettato.)
024500180308      *
024600180308     c                   if            WPKF = 0
024700180308     c                   exsr      VOL_PES
024800180308      * peso medio della spedizione diviso i singoli colli
024900180308     c                   if         arbNCL > 0
025000180308     C                   EVAL          WPKF= wpes/arbncl
025100180308     c                   end
025200180308     c                   end
025300180308      *
025400180308     C     WPKF          IFEQ      0
025500180308     C                   Z-ADD     1             WPKF
025600180308     C                   END
025700180308      *
025800180308     C                   clear                   iPKF             11
025900180308     c                   clear                   fld11            11
026000180308     C                   move      WPKF          dPKF              1
026100180308     c                   movel     wpkf          fld11
026200180308     C                   movea     fld11         Num
026300180308     C                   do        10            i                 3 0
026400180308     c                   if        Num(i)= '0' or Num(i)= ' '
026500180308     c                   eval      Num(i)=' '
026600180308     c                   else
026700180308     c                   leave
026800180308     c                   end
026900180308     c                   enddo
027000180308     C*
027100180308     C                   movea     Num           iPKF
027200180308     C*
027300180308     C*  Peso del singolo collo
027400180308     c                   clear                   Dati
027500180308     c                   eval      Dati = %Trim(dati_Bolla)
027600180308     c                   eval      Dati = %Trim(Dati) +
027700180308     c                             %Trim(iPKF) + puntodec + dPKF + virgola
027800180308      * FINE PESO        ------------   Weight
027900180308      *
028000180308      *  Rif.spedizione BRT
028100180308     c                   eval      Dati = %Trim(dati) + fldAAS +
028200180308     c                                fldLNP + fldNRS + fldNSP + virgola
028300180308     C* FINE RIF SPEDIZ. ------------   Reference (your reference)
028400180308      *
028500180308     C*----------------
028600180308      *  Codice Segnacollo
028700180308     c                   eval      Dati = %Trim(dati) +
028800180308     c                                    %Trim(arsNOT)
028900180308     C* FINE SEGNACOLLO  ------------   Barcode
029000180308     C* scrittura
029100180312     c                   eval      sumDAT = Dati
029200180312     C                   WRITE     RRIFCSUM
029300180312      *
029400180308      *--
029500180308     c                   end
029600180308      *                 *-----------------------*
029700180308     C     KARB          reade     FNART01L
029800180308     c                   enddo
029900020226     C*----------------
030000020219     c                   endsr
030100950428     C*****************************************************************
030200950428     C* OPERAZIONI INIZIALI
030300950428     C*****************************************************************
030400950428     C     *INZSR        BEGSR
030500010426     C*
030600960530     C* Definisco chiavi di accesso
030700950428     C     KARB          KLIST
030800960530     C                   KFLD                    KAAS
030900960530     C                   KFLD                    KLNP
031000960530     C                   KFLD                    KNRS
031100960530     C                   KFLD                    KNSP
031200020227      *
031300950428     C     KAR4          KLIST
031400960530     C                   KFLD                    KAAS
031500960530     C                   KFLD                    KLNP
031600960530     C                   KFLD                    KNRS
031700960530     C                   KFLD                    KNSP
031800960530     C                   KFLD                    KTRC
031900031210      *
032000031210     C     KAR5          KLIST
032100031210     C                   KFLD                    KAAS
032200031210     C                   KFLD                    KLNP
032300031210     C                   KFLD                    KNRS
032400031210     C                   KFLD                    KNSP
032500031210     C                   KFLD                    KTRD              3
032600020227      *
032700020226     C     KARS          KLIST
032800020226     C                   KFLD                    artfls
032900020226     C                   KFLD                    artlna
033000020226     C                   KFLD                    artnrs
033100180308     C                   KFLD                    artnsc
033200020226     C                   KFLD                    KTRC
033300020227      *
033400961126     C     KTAB1         KLIST
033500961126     C                   KFLD                    KKUT
033600961126     C                   KFLD                    KCOD
033700961126     C                   KFLD                    KKEY
033800020227      *
033900020227      *------------
034000960530     C*  Definisco variabili
034100960530     C     *LIKE         DEFINE    ARBAAS        KAAS
034200960530     C     *LIKE         DEFINE    ARBLNP        KLNP
034300960530     C     *LIKE         DEFINE    ARBNRS        KNRS
034400960530     C     *LIKE         DEFINE    ARBNSP        KNSP
034500960530     C     *LIKE         DEFINE    AR4TRC        KTRC
034600180307     C     *LIKE         DEFINE    AR4NOT        alfNOT
034700960530     C     *LIKE         DEFINE    TBLKUT        KKUT
034800960530     C     *LIKE         DEFINE    TBLCOD        KCOD
034900961126     C     *LIKE         DEFINE    TBLKEY        KKEY
035000020220     c     *like         define    arbRSD        fldRSD
035100020220     c     *like         define    arbind        fldind
035200020220     c     *like         define    arbLOD        fldLOD
035300030521     C     *LIKE         DEFINE    §qttpc        WTARA
035400180307     C     *LIKE         DEFINE    §ar5telD      fldTEL
035500180307
035600020220     C* Inizializzo variabili totale
035700960530     C* Imposto data del giorno
035800960530     C                   TIME                    W0140            14 0
035900960530     C                   MOVE      W0140         G02DAT            8 0
036000960530     C                   MOVEL     *BLANK        G02ERR
036100960530     C                   CALL      'XSRDA8'
036200960530     C                   PARM                    WLBDA8
036300960530     C                   MOVEL     G02INV        WOGGI             8 0
036400110512      *
036500030521     C** CARICAMENTO DATI VARI TASSAZIONE
036600030521     C                   Z-ADD     1             KKUT
036700030521     C                   MOVEL     'QT'          kcod
036800030521     C                   MOVEL     *BLANKS       kKEY
036900030521     C                   MOVEL     '1'           kKEY
037000030521     C     KTAB1         CHAIN     TABEL00F                           30
037100030521     C  N30              MOVEL     TBLUNI        DSQT1
037200020227      *------------
037300960530     C                   ENDSR
037400020227     C*****************************************************************
037500030317     C*  Reperisce Peso/Volume o da CML o da Bollettato
037600030317     C*****************************************************************
037700030317     C     VOL_PES       BEGSR
037800030317     C*
037900180308     c     *like         define    arbvlb        wvol
038000180308     c     *like         define    arbpkb        wpes
038100030317      *
038200030317      * Prima cerca il volume fra bollettato e CML
038300050112     C     arbncR        ifeq      arbncl
038400030317      * se è considerata un'unica spedizione
038500030317     C                   z-add     ARBVLC        WVOL
038600030317     C                   else
038700030317     C*
038800030317     C     ARBVLC        ifge      ARBVLb
038900030317     C                   z-add     ARBVLC        WVOL
039000030317     C                   else
039100030317     C                   z-add     ARBVLb        WVOL
039200030317     C                   end
039300030317     C*
039400030317     C                   endif
039500030521     C* PESO VDL - TARA
039600030521     C     §QTTPC        MULT      arbncp        WTARA
039700030521     C                   Z-ADD     arbPKC        wpes
039800030521     c                   sub(h)    wtara         wpes
039900030521      *
040000030317      * Poi cerca il peso fra bollettato e CML
040100050112     C     arbncP        ifeq      arbncl
040200030317      * se è considerata un'unica spedizione
040300030317     C                   ELSE
040400030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
040500030521     C     wpes          iflt      arbPKb
040600030317     C                   Z-ADD     arbPKb        wpes
040700030317     C                   end
040800030317     C*
040900030317     C                   ENDIF
041000030317     C*
041100030317     C                   ENDSR
041200030521     C*-----------------------------------------------------*
