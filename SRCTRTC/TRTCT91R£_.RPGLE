000100110516     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200110516     H BNDDIR('QC2LE')
000300110516     H DECEDIT('0,') DATEDIT(*YMD/)
000400110516      **?************************************************************************
000500110516      *  Da MANIFEST  deve ESPORTARE gli IFCSUM                                 *
000600110516      **?************************************************************************
000700110516      *   DECODIFICA e TRADUCE FLAT FILE MANIFEST EDI ESTERO                    *
000800110516      **?************************************************************************
000900110516      ***********************************************************************
001000110516      *  Partendo dal Flat file generato dal giro del MANIFEST              *
001100110516      *   preparato per il traduttore INTESA, questo programma scrive       *
001200110516      *   il TIVGD di DOWNLOAD per codice cliente seguendo le regole        *
001300110516      *   dell'IFCSUM....bypassando così il traduttore di INTESA.           *
001400110516      *   Viene creato un file che ha per record i singoli Segmenti.        *
001500110516      ***********************************************************************
001600110516     FEDIfcsum  iF   E             DISK    usropn  INFDS(SuMRECDS)
001700110525     Fedtab01l  iF   E           k DISK
001800110516      *
001900110516     I*---------------------------------------------------------------------*
002000110516      * Stringhe per conversione alfanumerico/numerico N.Documento
002100110516     D CA              S             78    DIM(1) CTDATA PERRCD(1)
002200110516     D CN              S             78    DIM(1) CTDATA PERRCD(1)
002300110516      **?************************************************************************
002400110516      * numero relativo di record
002500110516     D SuMRECDS        DS
002600110516     D  VNREC                397    400B 0
002700110516      *----------------------------------------------------*
002800110525     D* .. per scompozione dati ricevuti a seconda del tipo record
002900110525     D   TT00ds      E DS                  EXTNAME(EDTT00DS)
003000110525     D EDIDSpt       e ds
003100110525     D EDIDSpu       e ds
003200110525      *----------------------------------------------------*
003300110525     D Valori_inErr    ds
003400110516     D  SKout_Errori                  1    DIM(50)
003500110516      **
003600110516     D Descr_Errore    ds
003700110516     D  SKout_DesErr                 50    DIM(50)
003800110516      *----------------------------------------------------*
003900110516     D EoFEdSUM        s              1
004000110516     D*---------------------------------------------------------------------*
004100110516     D* .. per reperimento tipo record di EDIFCSUM da scrivere
004200110516     D*                    .. 5 - 8  : tipo record
004300110516     D WDAT            DS          1950
004400110516     D  SUMPRG                 1      4  0
004500110516     D  SUMTPR                 5      8
004600110516     D*---------------------------------------------------------------------*
004700110516     D W0140           S             14  0
004800110516     D DataGGMMAAA     S              8  0
004900110516     D DATEU_AMG       S              8  0
005000110516     D DATA_eur        S               D   DATFMT(*eur)
005100110516      *----------------------------------------------------*
005200110516      *  Invio E-mail
005300110516     D Indirizzi       s            253
005400110516     D Oggetto         s             44
005500110516     D Messaggio       s           5000
005600110516      *
005700110516     d   DSmail        ds
005800110516     D Mail_Ind                      44
005900110516     d   IND_char                     1    dim(44)
006000110516      *
006100110516     D Lunghezza_Indirizzo...
006200110516     D                 s              5u 0
006300110516     C*---------------------------------------------------------------*
006400110516     D MsgDSP          S            256                                         Testo generico
006500110516     C*---------------------------------------------------------------*
006600110516     D SndMg01         S             10                                         Message type
006700110516     D                                     INZ('*INFO')
006800110516     D SndMg02         S             10                                         Deluvery mode
006900110516     D                                     INZ('*BREAK')
007000110516     D SndMg03         S            256                                         Message text
007100110516     D SndMg04         S             10I 0                                      Length of text
007200110516     D                                     INZ(%SIZE(SndMg03))
007300110516     D SndMg05         S             10                                         User profile
007400110516     D                                     DIM(299)
007500110516     D SndMg06         S             10I 0                                      Number of user
007600110516     D SndMg07         S             10I 0                                      Message sent indic.
007700110516     D SndMg08         S             10I 0                                      Function requested
007800110516     D SndMg10         S              1                                         Show display
007900110516     D                                     INZ('N')
008000110516     D SndMg11         S             20                                         Qualified MSGQ name
008100110516     D SndMg12         S              4                                         Name type indicator
008200110516     D                                     INZ('*USR')
008300110516      * indice di scihera
008400110516     D Jx              S              5I 0
008500110516      *
008600110516     D/COPY QSYSINC/QRPGLESRC,QUSEC
008700110516      *
008800110516     d bart_it         C                   CONST('@Bartolini.it;')
008900110516     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
009000110516      * ?================================================================== */
009100110516      * ?   * Campi x decodifica * (INPUT  del Record)
009200110516      * ?================================================================== */
009300110516     D  Dati           s           2048
009400110516      * ?* ------------------------------------------------------ *
009500110516     D Digits          C                   '0123456789'
009600110516     D*------------------
009700110516     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009800110516     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009900110516      *----------------------------------------------------*
010000110516     I* FILE
010100110516     I*---------------------------------------------------------------------*
010200110516     IRRIFCSUM
010300110516     I              RRIFCSUM                    SUMDAT
010400110516      * ?================================================================== */
010500110516      *   Ciclo principale
010600110516      * ?================================================================== */
010700110516     c     *ENTRY        PLIST
010800110516     C                   parm                    esito             1
010900110525      *
011000110516     C                   eval      esito = '0'
011100110525     c                   eval      Compatta_80 = 'S'
011200110516      *
011300110516      * Recupero data e ora
011400110516      * UDATE IN GGMMAAAA
011500110516     C                   TIME                    W0140
011600110516     C                   MOVE      W0140         DataGGMMAAA
011700110516      * UDATE IN AAAAMMGG
011800110516     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
011900110516     C     *iso          MOVEL     DATA_eur      dateu_AMG
012000110516      *
012100110516      **  da EDIfcsum esegue la pretraduzione portando su DDS ogni record
012200110516     C                   if        not %open(EDIfcsum)
012300110516     C                   open      EDIfcsum
012400110516     C                   endif
012500110516      **
012600110516     c     *start        setll     EDIfcsum
012700110516      * ?              /*---------------------- */
012800110516     c                   EXSR      LEGGE_EdSUM_R
012900110516      * ?              /*---------------------- */
013000110516      **
013100110516     c                   dow       EoFEdSUM <> 'S'
013200110516      ** Decodifica riga
013300110516     C                   EXSR      Exit_Segmenti
013400110516
013500110516      * ?              /*---------------------- */
013600110516     c                   EXSR      LEGGE_EdSUM_R
013700110516      * ?              /*---------------------- */
013800110516     C                   ENDdo
013900110516      * chiude EdSUM
014000110516     C                   if        %open(EDIfcsum)
014100110516     C                   close     EDIfcsum
014200110516     C                   endif
014300110516      *
014400110516      *  programmi di traduzione
014500110516     c                   if          esito <> '0'
014600110516     c                   exsr      Invio_Mail
014700110516     C                   endif
014800110516      *
014900110516     c                   SETON                                        LR
015000110516      * ?================================================================== */
015100110516      *?    Legge il EdSUM eseguendo subito delle operazioni Essenziali
015200110516      * ?================================================================== */
015300110516     C     LEGGE_EdSUM_R BEGSR
015400110516      *
015500110516     c                   clear                   EoFEdSUM
015600110516      *  Lettura sequenziale
015700110516     c                   read      EDIfcsum
015800110516
015900110516     c                   if        %Eof(EDIfcsum)
016000110516      *
016100110516     c                   move      'S'           EoFEdSUM
016200110520      ** Decodifica riga
016300110520     C                   if          Numero_Righe > 1
016400110520     c                   eval      SUMTPR = 'FINE'
016500110520     C                   EXSR      Exit_Segmenti
016600110520     c                   end
016700110516      *
016800110516     c                   else
016900110516      *
017000110516      * ?  imposta il tipo di segmento:
017100110516     c                   clear                   dati
017200110516     C     minu:maiu     XLATE     SuMDAT        dati                           *Minus -> Maiuscolo
017300110516     c                   eval      Wdat = %trimL(dati)
017400110516     c                   end
017500110516      *
017600110525      *  se testata decodifica informazioni sul destinatario del messaggio
017700110525     c                   if           SUMTPR  = 'TT00'
017800110525     c                   exsr      dest_messaggio
017900110525     c                   end
018000110525     c*
018100110525     C                   ENDSR
018200110525      *
018300110525      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
018400110525      *  Decodifica il destinatario con tab.PT e PU
018500110525      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
018600110525     C     Dest_MessaggioBEGSR
018700110525      *
018800110525     c     keytab        klist
018900110525     c                   kfld                    tabcod
019000110525     c                   kfld                    tabkey
019100110525      *
019200110525     c                   eval      tt00ds = SuMDAT
019300110525     c                   movel     'PT'          tabcod
019400110525     c                   eval       tabkey = %subst(TA0010:1:31)
019500110525      *
019600110525     c     keytab        setll     edTAB01l
019700110525     c     tabcod        reade     edTAB01l
019800110525      **
019900110525     c                   dow       not %Eof(edTAB01l)
020000110525      ** Basta il Parziale
020100110525     c                   if         %subst(tabKEY:1:31) = %subst(TA0010:1:31)
020200110525      *   Valorizza la DS
020300110525     c                   eval      edidsPT = tabUNI
020400110525     c                   move      *all'0'       CLIENTE
020500110525     c                   move      §ptKSC        CLIENTE
020600110525      *
020700110525     c                   movel     'PU'          tabcod
020800110525     c     keytab        chain     edTAB01l
020900110525      *
021000110525     c                   if        %Found(edTAB01l)
021100110525     c                   eval      edidsPU = tabUNI
021200110525     c                   eval      Compatta_80 = §pucompat
021300110525     c                   eval      Suffisso    = §puSIG
021400110525     c                   end
021500110525      **
021600110525      ** Comunque esce dal ciclo
021700110525     c                   leave
021800110525     c                   end
021900110525      **
022000110525     c     tabcod        reade     edTAB01l
022100110525     c                   endDo
022200110525     c*
022300110516     C                   ENDSR
022400110516      *
022500110516      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
022600110516      *  Scrittura dei segmenti IFCSUM S93A
022700110516      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
022800110516      *
022900110516     C     Exit_Segmenti BEGSR
023000110516      *
023100110516     c                   clear                   DsGenerica
023200110516     c                   eval      Tipo_DS    = SUMTPR
023300110516     c                   eval      DsGenerica = SUMDAT
023400110516     c                   call      'TRTCT91R1'
023500110525     c                   PARM                    Compatta_80       1
023600110525     c                   PARM                    CLIENTE           8
023700110516     c                   PARM                    Suffisso          2
023800110516     c                   PARM                    Tipo_DS           4
023900110516     c                   PARM                    DsGenerica     1950
024000110516     C                   PARM                    Numero_Righe      5 0
024100110617     C                   PARM                    Progressivo      10
024200110621     c                   PARM                    esito             1
024300110516      *
024400110516     C                   ENDSR
024500110516      *
024600110516      * ?================================================================== */
024700110516      *?      X non bloccare in nessun caso il traduttore CLIENTI
024800110516      * ?================================================================== */
024900110516     C     *pssr         BEGSR
025000110516     C
025100110516     C                   eval      esito = '2'
025200110516???? C                   EVAL      Oggetto ='Problemi DOWNLD MANIFEST EDI !!'
025300110707     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
025400110516     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
025500110707     C                             MANIFEST NON tradotto x invio DOWNLD. -
025600110516     C                             Controllare il Download del Cliente.'
025700110707     C                   EVAL      Messaggio = %trim(Messaggio) +
025800110707     C                                         %editc(§PTksc:'X')
025900110516     c                   exsr      Invio_Mail
026000110516     C
026100110516     C                   ENDSR
026200110516     c*==================================================================*
026300110516      * Manda un Msg x E-mail
026400110516     c*==================================================================*
026500110516     c     Invio_Mail    begsr
026600110516      *
026700110516???? C                   EVAL      Oggetto ='Problemi DOWNLD MANIFEST EDI !!'
026800110707     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
026900110516     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
027000110516     C                             MANIFEST NON tradotto correttamente. -
027100110516     C                             Controllare il Download del Cliente.'
027200110707     C                   EVAL      Messaggio = %trim(Messaggio) +
027300110707     C                                         %editc(§PTksc:'X')
027400110516      *
027500110516     c                   exsr      SEND_MSG_EDP
027600110516      *
027700110516     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
027800110516     C* Inizializzo variabili
027900110516     C                   movel     *blanks       wrkEml          253
028000110516     C                   movel     *blanks       wrkMsg         5000
028100110516     C                   movel     *blanks       wrkOgg           44
028200110516      *
028300110516     C* Valorizzo i campi della e-m@ail - indirizzo
028400110516     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
028500110516     C                   eval      wrkEml = CED_BART
028600110516     C                   eval      wrkOgg = Oggetto
028700110516     C                   eval      wrkMsg = Messaggio
028800110516      *
028900110516     C                   call(e)   'TRTCT00R2'
029000110516     C                   parm                    wrkEml
029100110516     C                   parm                    wrkOgg
029200110516     C                   parm                    wrkMsg
029300110516     C*
029400110516     C*
029500110516     C                   ENDSR
029600110516     ***********************************************************************
029700110516     ** Send Message (QEZSNDMG) API
029800110516     ***********************************************************************
029900110516     C     CalQEZSNDMG   BEGSR
030000110516      *
030100110516     ** Invio messaggio all'utente.
030200110516     C                   EVAL      SndMg03 = msgDSP
030300110516     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
030400110516     C                   EVAL      SndMg05(Jx) = CED_BART
030500110516     C                   EVAL      SndMg06 = Jx
030600110516     C                   CLEAR                   QUSEC
030700110516     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
030800110516
030900110516     C                   CALL      'QEZSNDMG'
031000110516     C                   PARM                    SndMg01
031100110516     C                   PARM                    SndMg02
031200110516     C                   PARM                    SndMg03
031300110516     C                   PARM                    SndMg04
031400110516     C                   PARM                    SndMg05
031500110516     C                   PARM                    SndMg06
031600110516     C                   PARM                    SndMg07
031700110516     C                   PARM                    SndMg08
031800110516     C                   PARM                    Qusec
031900110516     C                   PARM                    SndMg10
032000110516     C                   PARM                    SndMg11
032100110516     C                   PARM                    SndMg12
032200110516      *
032300110516     C                   ENDSR
032400110516     c*==================================================================*
032500110516      * Manda un Msg in Posta AS Sede a EDP*
032600110516     c*==================================================================*
032700110516     c     SEND_MSG_EDP  begsr
032800110516      *
032900110516      * INVIA MESSAGGIO ad un Utente --> EDPAB
033000110516      *   Lo manda una sola volta per lavoro
033100110516     c                   z-add     1             Jx
033200110516     C                   exsr      CalQEZSNDMG
033300110516      *
033400110516     c                   endsr
033500110516     c*
033600110516
033700110516** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
033800110516ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
033900110516** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
034000110516987654321098765432109876540123456789987654321098765432109876540999999999999999 1
