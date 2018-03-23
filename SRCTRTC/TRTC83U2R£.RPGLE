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
001300180307     FFifedukf  O  A E             DISK
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
004200020222     C*---------------------------------------------------------------------*
004300960530     C* Ciclo principale                                                   -*
004400960530     C*---------------------------------------------------------------------*
004500010426     C     *ENTRY        PLIST
004600010426     C                   PARM                    KPJBA
004700180312     C                   MOVEL     KPJBU         TRTC82DS
004800180308     C* scrittura  testata
004900180308     c                   if        testata = *blank
005000180308     c                   eval      FEDUKBOL = 'Recipient Name,Recipient Company-
005100180312     c                             ,Recipient Address 1,Recipient Address 2,Rec-
005200180308     c                             ipient Post Town,Recipient Post Code,Recipie-
005300180308     c                             nt Telephone,Items,Weight,Bartolini Referenc-
005400180308     c                             e,Bartolini Barcode'
005500180308     c
005600180308     C                   write     FifedUK0
005700180308     c                   move      'S'           TESTATA           1
005800180308     c                   end
005900180307     C*----
006000960530     C*  Se c'è numero spedizione scrivo dettaglio
006100180312    2C     D82NSP        IFNE      0
006200180307     C*----
006300960530     C*  Eseguo posizionamento su archivio bolle per passaggio param.
006400180312     C                   Z-ADD     D82AAS        KAAS
006500180312     C                   Z-ADD     D82LNP        KLNP
006600180312     C                   Z-ADD     D82NRS        KNRS
006700180312     C                   Z-ADD     D82NSP        KNSP
006800020220      * B O L L A
006900960530     C     KARB          CHAIN     FNARB01L                           31
007000020219      *
007100970124    3C     *IN31         IFEQ      *OFF
007200180308      * ------------
007300020220     C* COMPOSIZIONE record BOLLA
007400020219     C* dati bolla
007500180307     C                   clear                   Dati
007600180307     C                   clear                   Dati_Bolla
007700180308     c                   exsr      dati_SPEDIZ
007800180308     c                   exsr      dati_COLLO
007900970124    3C                   END
008000020219     C*
008100960530     C*  Fleggo FNARB come trasmesso
008200180312     c                   if        D82DFV >0
008300960530     C                   MOVEL     'T'           ARBFT3
008400180312     C                   Z-ADD     D82DFV        ARBDT3
008500020227     c                   end
008600020227     C*
008700020227     C                   update    fnarb000
008800970124     C                   SETON                                        RT
008900970124     C*
009000970124   X2C                   ELSE
009100020219     C*
009200970124     C                   SETON                                        LR
009300020227     C*
009400970124    2C                   END
009500960530     C*--------------------------------------------------------------*
009600020219     C* dettaglio spedizione
009700960530     C*--------------------------------------------------------------*
009800180308     C     Dati_SPEDIZ   BEGSR
009900180308     C*
010000180308     c                   move      arbaas        fldaas            4
010100180308     c                   move      arblnp        fldlnp            3
010200180308     c                   move      arbnrs        fldnrs            2
010300180308     c                   move      arbnsp        fldnsp            7
010400180308     C*
010500020219     C* Concatena tutte le informazioni da inserire nel record
010600180307      *       FEDEX vuole queste informazioni separate da virgola poichè è un CSV
010700180307      * .         Recipient Contact Name
010800180307      * ·         Recipient Company Name
010900180307      * ·         Recipient Address Line 1
011000180307      * ·         Recipient Address Line 2
011100180307      * ·         Recipient Post Town
011200180307      * ·         Recipient Post Code
011300180307      * ·         Recipient Telephone (0 if not known)
011400180307      * ·         Items (always 1)
011500180308      * ·--         Weight
011600180308      * ·--         Reference (your reference)
011700180308      * ·--         Barcode content (this must be an exact match
011800180308      *               between the content of the unique barcode on the box,
011900180308      *                 as it presumably must be today)
012000180307      *--------
012100180307      * Nome Destinatario
012200180307     c     ',':' '       xlate     arbRSD        fldRSD
012300180307     c                   eval      Dati =  %Trim(fldRSD)
012400180307      **---
012500180307     C*  Reperisco seconda ragione sociale destinatario
012600180307     C*    attenzione x Bartolini la rag.soc. è su 2 righe x tot.70 chars.
012700180307     C*     ed il secondo pezzo è su fiar4.......
012800180307     C*   per identificare correttamente il nominativo
012900180307     C*  del destinatario straniero.
013000180307     C* Si può effettuare un concatenamento delle informazioni.
013100180307     C                   MOVEL     'D'           KTRC
013200180307     c                   clear                   fldNOT          256
013300180307     c                   clear                   alfNOT
013400180307     C     KAR4          CHAIN     fiar401L
013500180307     c                   if        %Found(fiar401L)
013600180307     c     ',':' '       xlate     ar4not        alfNOT
013700180307     c                   eval      fldnot =%Trim(alfnot)
013800180307     c                   end
013900180307      *
014000180307      *  ma se c'è il riferimento destinatario del FIAR5 prende anche
014100180307      *   questo e lo accoda al dato presente su AR4 cercando al meglio
014200180307      *    di dare il più possibile l'informazione.
014300180307     c                   clear                   DAr5Gen
014400180307     c                   clear                   fldTEL
014500180307     c                   movel     'GEN'         ktrd
014600180307     c     kar5          chain     fiar501L
014700180307     c                   if        %Found(fiar501L)
014800180307     c                   Movel     Ar5Uni        DAr5Gen
014900180307      *--  Telefono referente di consegna
015000180307     c                   if         §ar5telD <> *blank
015100180307     c                   eval      fldTEL = %Trim(§ar5telD)
015200180307     c                   end
015300180307      *--  Referente di consegna
015400180307     c                   if         §ar5ref <> *blank
015500180307     c                   if          fldNOT <> *blank
015600180307     c                   eval      fldnot = %Trim(fldnot) + %Trim('-') +
015700180307     c                             %Trim(§ar5ref)
015800180307     c                   else
015900180307     c                   eval      fldnot = %Trim(§ar5ref)
016000180307     c                   end
016100180307     c                   end
016200180307      *--
016300180307     c                   end
016400180307      *--
016500180307      * Imposta comunque se l'ha trovato un riferimento destinatario
016600180307     c                   if        fldnot <> *blank
016700180307     c                   eval      Dati = %Trim(dati) + '-' +
016800180307     c                                    %Trim(fldnot)
016900180307     c                   end
017000180307      *-
017100180307     c                   eval      Dati = %Trim(dati) + virgola
017200180308      *-
017300180307     C* FINE RAGIONE SOC ------------   Recipient Contact Name
017400180307      *-
017500180308      * Company name
017600180307     c                   eval      Dati = %Trim(dati) + virgola
017700180307     C* FINE COMPANY NAME -----------   Recipient Company Name
017800180307      *--
017900180307     C* Indirizzo Destinatario
018000180307     c     ',':' '       xlate     arbind        fldind
018100180307     c                   eval      Dati = %Trim(dati) +
018200180307     c                                    %Trim(fldind) + virgola
018300180307     C* FINE INDIRIZZO 1 ------------   Recipient Address Line 1
018400180307      *-
018500180307     c                   eval      Dati = %Trim(dati) + virgola
018600180307     C*    E INDIRIZZO 2 ------------   Recipient Address Line 2
018700180307      *
018800180307      * Città
018900180307     c     ',':' '       xlate     arbLOD        fldLOD
019000180307     c                   eval      Dati = %Trim(dati) +
019100180307     c                                    %Trim(fldLOD) + virgola
019200180307      *
019300180307     C* FINE LOCALITA'   ------------   Recipient Post Town
019400180307      *
019500180307      * Zip Code
019600180307     c                   eval      Dati = %Trim(dati) +
019700180307     c                                    %Trim(arbCAD) + virgola
019800180307     C* FINE CAP         ------------   Recipient Post Code
019900180307      *
020000180307      * Telefono del Destinatario
020100180307     c                   if        fldTEL = *blank
020200180307     c                   eval      Dati = %Trim(dati) +
020300180307     c                                    '0' + virgola
020400180307     c                   else
020500180307     c                   eval      Dati = %Trim(dati) +
020600180307     c                                    %Trim(fldTEL) + virgola
020700180307     c                   end
020800180307     C* FINE TELEFONO    ------------   Recipient Telephone (0 if not known)
020900180307      *
021000180307      * Item è sempre =1
021100180307     c                   eval      Dati = %Trim(dati) +
021200180307     c                                    '1' + virgola
021300180307     C* FINE ITEM        ------------   Items (always 1)
021400180307      *
021500180308      *   Salva la parte di dati comuni da ripetere per ogni collo
021600180308     c                   eval        Dati_Bolla = Dati
021700180308      ***
021800180308     c                   endSR
021900180308      * ------------------------------------------------------------------- *
022000180308     C*   Dati singolo collo della spedizione (Tante righe per ogni collo)
022100180308      * ------------------------------------------------------------------- *
022200180308     c     Dati_COLLO    begSR
022300180308      *--
022400180308     C     KARB          setll     FNART01L
022500180308     C     KARB          reade     FNART01L
022600180308      *-
022700180308     c                   dow       not %EoF(FNART01L)
022800180308      *                 *-----------------------*
022900180308     C                   Z-ADD(H)  artPUC        WPKF             12 1
023000180308      *                 *-----------------------*
023100180308     C*  segnacollo
023200180308     C                   move      'E'           KTRC
023300180308     C     KARS          chain     fIARS01L
023400180308     c                   if        %found(fIARS01L)
023500180308      *-
023600180308     C*  prende il volume/peso da CML o da Bollettazione dividendo per i colli
023700180308      *  della spedizione
023800180308      *   (il Peso deve essere quello reale preso o da CML o da Bollettato.)
023900180308      *
024000180308     c                   if            WPKF = 0
024100180308     c                   exsr      VOL_PES
024200180308      * peso medio della spedizione diviso i singoli colli
024300180308     c                   if         arbNCL > 0
024400180308     C                   EVAL          WPKF= wpes/arbncl
024500180308     c                   end
024600180308     c                   end
024700180308      *
024800180308     C     WPKF          IFEQ      0
024900180308     C                   Z-ADD     1             WPKF
025000180308     C                   END
025100180308      *
025200180308     C                   clear                   iPKF             11
025300180308     c                   clear                   fld11            11
025400180308     C                   move      WPKF          dPKF              1
025500180308     c                   movel     wpkf          fld11
025600180308     C                   movea     fld11         Num
025700180308     C                   do        10            i                 3 0
025800180308     c                   if        Num(i)= '0' or Num(i)= ' '
025900180308     c                   eval      Num(i)=' '
026000180308     c                   else
026100180308     c                   leave
026200180308     c                   end
026300180308     c                   enddo
026400180308     C*
026500180308     C                   movea     Num           iPKF
026600180308     C*
026700180308     C*  Peso del singolo collo
026800180308     c                   clear                   Dati
026900180308     c                   eval      Dati = %Trim(dati_Bolla)
027000180308     c                   eval      Dati = %Trim(Dati) +
027100180308     c                             %Trim(iPKF) + puntodec + dPKF + virgola
027200180308      * FINE PESO        ------------   Weight
027300180308      *
027400180308      *  Rif.spedizione BRT
027500180308     c                   eval      Dati = %Trim(dati) + fldAAS +
027600180308     c                                fldLNP + fldNRS + fldNSP + virgola
027700180308     C* FINE RIF SPEDIZ. ------------   Reference (your reference)
027800180308      *
027900180308     C*----------------
028000180308      *  Codice Segnacollo
028100180308     c                   eval      Dati = %Trim(dati) +
028200180308     c                                    %Trim(arsNOT)
028300180308     C* FINE SEGNACOLLO  ------------   Barcode
028400180308     C* scrittura
028500180308     c                   eval      FEDUKBOL = Dati
028600180308     C                   write     FifedUK0
028700180308      *--
028800180308     c                   end
028900180308      *                 *-----------------------*
029000180308     C     KARB          reade     FNART01L
029100180308     c                   enddo
029200020226     C*----------------
029300020219     c                   endsr
029400950428     C*****************************************************************
029500950428     C* OPERAZIONI INIZIALI
029600950428     C*****************************************************************
029700950428     C     *INZSR        BEGSR
029800010426     C*
029900960530     C* Definisco chiavi di accesso
030000950428     C     KARB          KLIST
030100960530     C                   KFLD                    KAAS
030200960530     C                   KFLD                    KLNP
030300960530     C                   KFLD                    KNRS
030400960530     C                   KFLD                    KNSP
030500020227      *
030600950428     C     KAR4          KLIST
030700960530     C                   KFLD                    KAAS
030800960530     C                   KFLD                    KLNP
030900960530     C                   KFLD                    KNRS
031000960530     C                   KFLD                    KNSP
031100960530     C                   KFLD                    KTRC
031200031210      *
031300031210     C     KAR5          KLIST
031400031210     C                   KFLD                    KAAS
031500031210     C                   KFLD                    KLNP
031600031210     C                   KFLD                    KNRS
031700031210     C                   KFLD                    KNSP
031800031210     C                   KFLD                    KTRD              3
031900020227      *
032000020226     C     KARS          KLIST
032100020226     C                   KFLD                    artfls
032200020226     C                   KFLD                    artlna
032300020226     C                   KFLD                    artnrs
032400180308     C                   KFLD                    artnsc
032500020226     C                   KFLD                    KTRC
032600020227      *
032700961126     C     KTAB1         KLIST
032800961126     C                   KFLD                    KKUT
032900961126     C                   KFLD                    KCOD
033000961126     C                   KFLD                    KKEY
033100020227      *
033200020227      *------------
033300960530     C*  Definisco variabili
033400960530     C     *LIKE         DEFINE    ARBAAS        KAAS
033500960530     C     *LIKE         DEFINE    ARBLNP        KLNP
033600960530     C     *LIKE         DEFINE    ARBNRS        KNRS
033700960530     C     *LIKE         DEFINE    ARBNSP        KNSP
033800960530     C     *LIKE         DEFINE    AR4TRC        KTRC
033900180307     C     *LIKE         DEFINE    AR4NOT        alfNOT
034000960530     C     *LIKE         DEFINE    TBLKUT        KKUT
034100960530     C     *LIKE         DEFINE    TBLCOD        KCOD
034200961126     C     *LIKE         DEFINE    TBLKEY        KKEY
034300020220     c     *like         define    arbRSD        fldRSD
034400020220     c     *like         define    arbind        fldind
034500020220     c     *like         define    arbLOD        fldLOD
034600030521     C     *LIKE         DEFINE    §qttpc        WTARA
034700180307     C     *LIKE         DEFINE    §ar5telD      fldTEL
034800180307
034900020220     C* Inizializzo variabili totale
035000960530     C* Imposto data del giorno
035100960530     C                   TIME                    W0140            14 0
035200960530     C                   MOVE      W0140         G02DAT            8 0
035300960530     C                   MOVEL     *BLANK        G02ERR
035400960530     C                   CALL      'XSRDA8'
035500960530     C                   PARM                    WLBDA8
035600960530     C                   MOVEL     G02INV        WOGGI             8 0
035700110512      *
035800030521     C** CARICAMENTO DATI VARI TASSAZIONE
035900030521     C                   Z-ADD     1             KKUT
036000030521     C                   MOVEL     'QT'          kcod
036100030521     C                   MOVEL     *BLANKS       kKEY
036200030521     C                   MOVEL     '1'           kKEY
036300030521     C     KTAB1         CHAIN     TABEL00F                           30
036400030521     C  N30              MOVEL     TBLUNI        DSQT1
036500020227      *------------
036600960530     C                   ENDSR
036700020227     C*****************************************************************
036800030317     C*  Reperisce Peso/Volume o da CML o da Bollettato
036900030317     C*****************************************************************
037000030317     C     VOL_PES       BEGSR
037100030317     C*
037200180308     c     *like         define    arbvlb        wvol
037300180308     c     *like         define    arbpkb        wpes
037400030317      *
037500030317      * Prima cerca il volume fra bollettato e CML
037600050112     C     arbncR        ifeq      arbncl
037700030317      * se è considerata un'unica spedizione
037800030317     C                   z-add     ARBVLC        WVOL
037900030317     C                   else
038000030317     C*
038100030317     C     ARBVLC        ifge      ARBVLb
038200030317     C                   z-add     ARBVLC        WVOL
038300030317     C                   else
038400030317     C                   z-add     ARBVLb        WVOL
038500030317     C                   end
038600030317     C*
038700030317     C                   endif
038800030521     C* PESO VDL - TARA
038900030521     C     §QTTPC        MULT      arbncp        WTARA
039000030521     C                   Z-ADD     arbPKC        wpes
039100030521     c                   sub(h)    wtara         wpes
039200030521      *
039300030317      * Poi cerca il peso fra bollettato e CML
039400050112     C     arbncP        ifeq      arbncl
039500030317      * se è considerata un'unica spedizione
039600030317     C                   ELSE
039700030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
039800030521     C     wpes          iflt      arbPKb
039900030317     C                   Z-ADD     arbPKb        wpes
040000030317     C                   end
040100030317     C*
040200030317     C                   ENDIF
040300030317     C*
040400030317     C                   ENDSR
040500030521     C*-----------------------------------------------------*
