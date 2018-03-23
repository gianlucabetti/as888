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
003200120416     D EDIDSpz       e ds
003300110525      *----------------------------------------------------*
003400110525     D Valori_inErr    ds
003500110516     D  SKout_Errori                  1    DIM(50)
003600110516      **
003700110516     D Descr_Errore    ds
003800110516     D  SKout_DesErr                 50    DIM(50)
003900110516      *----------------------------------------------------*
004000110516     D EoFEdSUM        s              1
004100110516     D*---------------------------------------------------------------------*
004200110516     D* .. per reperimento tipo record di EDIFCSUM da scrivere
004300110516     D*                    .. 5 - 8  : tipo record
004400110516     D WDAT            DS          1950
004500110516     D  SUMPRG                 1      4  0
004600110516     D  SUMTPR                 5      8
004700110516     D*---------------------------------------------------------------------*
004800110516     D W0140           S             14  0
004900110516     D DataGGMMAAA     S              8  0
005000110516     D DATEU_AMG       S              8  0
005100110516     D DATA_eur        S               D   DATFMT(*eur)
005200140506      * ?------------------------------------------------------------------ */
005300140506      * ?-  Parametri x controllo set di Caratteri con XCHKCHAR x CED INTESA
005400140506      * ?------------------------------------------------------------------ */
005500140506     D TxtInOut        S           2048
005600140506     D ElencoChar      S            256
005700140506     D TipoElenco      S              1
005800140506     D CharSost        S              1
005900140506     D UpperCase       S              1
006000140506     D ChkNull         S              1
006100140506     D CharNull        S              1
006200140506     D Esito_check     S              1
006300140506      * ?------------------------------------------------------------------ */
006400140506      * ?controllo x CEDINTESA
006500140506     D SETA            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
006600140506     D                                     3456789 .,-()/=''+:?!"%&*;<>')
006700140506      * ?------------------------------------------------------------------ */
006800110516      *  Invio E-mail
006900110516     D Indirizzi       s            253
007000110516     D Oggetto         s             44
007100110516     D Messaggio       s           5000
007200110516      *
007300110516     d   DSmail        ds
007400110516     D Mail_Ind                      44
007500110516     d   IND_char                     1    dim(44)
007600110516      *
007700110516     D Lunghezza_Indirizzo...
007800110516     D                 s              5u 0
007900110516     C*---------------------------------------------------------------*
008000110516     D MsgDSP          S            256                                         Testo generico
008100110516     C*---------------------------------------------------------------*
008200110516     D SndMg01         S             10                                         Message type
008300110516     D                                     INZ('*INFO')
008400110516     D SndMg02         S             10                                         Deluvery mode
008500110516     D                                     INZ('*BREAK')
008600110516     D SndMg03         S            256                                         Message text
008700110516     D SndMg04         S             10I 0                                      Length of text
008800110516     D                                     INZ(%SIZE(SndMg03))
008900110516     D SndMg05         S             10                                         User profile
009000110516     D                                     DIM(299)
009100110516     D SndMg06         S             10I 0                                      Number of user
009200110516     D SndMg07         S             10I 0                                      Message sent indic.
009300110516     D SndMg08         S             10I 0                                      Function requested
009400110516     D SndMg10         S              1                                         Show display
009500110516     D                                     INZ('N')
009600110516     D SndMg11         S             20                                         Qualified MSGQ name
009700110516     D SndMg12         S              4                                         Name type indicator
009800110516     D                                     INZ('*USR')
009900110516      * indice di scihera
010000110516     D Jx              S              5I 0
010100110516      *
010200110516     D/COPY QSYSINC/QRPGLESRC,QUSEC
010300110516      *
010400110516     d bart_it         C                   CONST('@Bartolini.it;')
010500110516     d CED_Bart        C                   CONST('CEDAlert@Bartolini.it;')
010600110516      * ?================================================================== */
010700110516      * ?   * Campi x decodifica * (INPUT  del Record)
010800110516      * ?================================================================== */
010900110516     D  Dati           s           2048
011000110516      * ?* ------------------------------------------------------ *
011100110516     D Digits          C                   '0123456789'
011200110516     D*------------------
011300140506      * ?set caratteri Maiuscoli/Minuscoli
011400110516     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011500110516     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011600110516      *----------------------------------------------------*
011700110516     I* FILE
011800110516     I*---------------------------------------------------------------------*
011900110516     IRRIFCSUM
012000110516     I              RRIFCSUM                    SUMDAT
012100110516      * ?================================================================== */
012200110516      *   Ciclo principale
012300110516      * ?================================================================== */
012400110516     c     *ENTRY        PLIST
012500110516     C                   parm                    esito             1
012600110525      *
012700110516     C                   eval      esito = '0'
012800110525     c                   eval      Compatta_80 = 'S'
012900110516      *
013000110516      * Recupero data e ora
013100110516      * UDATE IN GGMMAAAA
013200110516     C                   TIME                    W0140
013300110516     C                   MOVE      W0140         DataGGMMAAA
013400110516      * UDATE IN AAAAMMGG
013500110516     C     *eur          MOVEL     DataGGMMAAA   DATA_eur
013600110516     C     *iso          MOVEL     DATA_eur      dateu_AMG
013700110516      *
013800110516      **  da EDIfcsum esegue la pretraduzione portando su DDS ogni record
013900110516     C                   if        not %open(EDIfcsum)
014000110516     C                   open      EDIfcsum
014100110516     C                   endif
014200110516      **
014300110516     c     *start        setll     EDIfcsum
014400110516      * ?              /*---------------------- */
014500110516     c                   EXSR      LEGGE_EdSUM_R
014600110516      * ?              /*---------------------- */
014700110516      **
014800110516     c                   dow       EoFEdSUM <> 'S'
014900110516      ** Decodifica riga
015000110516     C                   EXSR      Exit_Segmenti
015100110516
015200110516      * ?              /*---------------------- */
015300110516     c                   EXSR      LEGGE_EdSUM_R
015400110516      * ?              /*---------------------- */
015500110516     C                   ENDdo
015600110516      * chiude EdSUM
015700110516     C                   if        %open(EDIfcsum)
015800110516     C                   close     EDIfcsum
015900110516     C                   endif
016000110516      *
016100110516      *  programmi di traduzione
016200110516     c                   if          esito <> '0'
016300110516     c                   exsr      Invio_Mail
016400110516     C                   endif
016500110516      *
016600110516     c                   SETON                                        LR
016700110516      * ?================================================================== */
016800110516      *?    Legge il EdSUM eseguendo subito delle operazioni Essenziali
016900110516      * ?================================================================== */
017000110516     C     LEGGE_EdSUM_R BEGSR
017100110516      *
017200110516     c                   clear                   EoFEdSUM
017300110516      *  Lettura sequenziale
017400110516     c                   read      EDIfcsum
017500110516
017600110516     c                   if        %Eof(EDIfcsum)
017700110516      *
017800110516     c                   move      'S'           EoFEdSUM
017900110520      ** Decodifica riga
018000110520     C                   if          Numero_Righe > 1
018100110520     c                   eval      SUMTPR = 'FINE'
018200110520     C                   EXSR      Exit_Segmenti
018300110520     c                   end
018400110516      *
018500110516     c                   else
018600140506      *
018700110516      * ?  imposta il tipo di segmento:
018800110516     c                   clear                   dati
018900110516     C     minu:maiu     XLATE     SuMDAT        dati                           *Minus -> Maiuscolo
019000140507      *
019100140507      *  IMPOSTA I CARATTERI del Syntax Version (2)
019200140507      *                *------------------------*
019300140507     c                   exsr      Syntax_Vers2
019400140507      *                *------------------------*
019500140507      *
019600110516     c                   eval      Wdat = %trimL(dati)
019700110516     c                   end
019800110516      *
019900110525      *  se testata decodifica informazioni sul destinatario del messaggio
020000110525     c                   if           SUMTPR  = 'TT00'
020100110525     c                   exsr      dest_messaggio
020200110525     c                   end
020300140506      *
020400140506     C                   ENDSR
020500140506      *
020600140506      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
020700140506      * Versione UNB+UNOA:2+ <--
020800140506      *
020900140506      * *------------------------*
021000140506     C     Syntax_Vers2  begSR
021100140506      * *------------------------*
021200140506      *
021300140507     c                   movel     dati          TxtInOut
021400140506     c                   eval      ElencoChar = SETA
021500140506     c                   eval      TipoElenco = *blank
021600140506     c                   eval      CharSost   = *blank
021700140506     c                   eval      UpperCase  = '1'
021800140506     c                   eval      ChkNull    = *blank
021900140506     c                   eval      CharNull   = *blank
022000140506     c                   eval      Esito_check= *blank
022100140506      *
022200140506     c                   call      'XCHKCHAR'
022300140506     c                   parm                    TxtInOut
022400140506     c                   parm                    ElencoChar
022500140506     c                   parm                    TipoElenco
022600140506     c                   parm                    CharSost
022700140506     c                   parm                    UpperCase
022800140506     c                   parm                    ChkNull
022900140506     c                   parm                    CharNull
023000140506     c                   parm                    Esito_check
023100140506      *
023200140506      *   se è stato modificato qualcosa nella riga di FLAT
023300140506     c                   if        Esito_check = '1'
023400140507     c                   movel     TxtInOut      dati
023500140506     c                   end
023600140506      *
023700110525     C                   ENDSR
023800110525      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
023900110525      *  Decodifica il destinatario con tab.PT e PU
024000110525      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
024100110525     C     Dest_MessaggioBEGSR
024200110525      *
024300110525     c     keytab        klist
024400110525     c                   kfld                    tabcod
024500110525     c                   kfld                    tabkey
024600110525      *
024700110525     c                   eval      tt00ds = SuMDAT
024800110525     c                   movel     'PT'          tabcod
024900110525     c                   eval       tabkey = %subst(TA0010:1:31)
025000110525      *
025100110525     c     keytab        setll     edTAB01l
025200110525     c     tabcod        reade     edTAB01l
025300110525      **
025400110525     c                   dow       not %Eof(edTAB01l)
025500110525      ** Basta il Parziale
025600110525     c                   if         %subst(tabKEY:1:31) = %subst(TA0010:1:31)
025700110525      *   Valorizza la DS
025800110525     c                   eval      edidsPT = tabUNI
025900110525     c                   move      *all'0'       CLIENTE
026000110525     c                   move      §ptKSC        CLIENTE
026100110525      *
026200110525     c                   movel     'PU'          tabcod
026300110525     c     keytab        chain     edTAB01l
026400110525      *
026500110525     c                   if        %Found(edTAB01l)
026600110525     c                   eval      edidsPU = tabUNI
026700110525     c                   eval      Compatta_80 = §pucompat
026800110525     c                   eval      Suffisso    = §puSIG
026900120416      **
027000120416      * se richiesta la trasmissione ad un codice UNIFICANTE PARTICOLARE
027100120416      **   deve sovrapporsi al codice cliente Generico legato all'UNB
027200120416     c                   if        §puXXX = 'S'
027300120416      *
027400120416     c                   movel     'PZ'          tabcod
027500120416     c     keytab        chain     edTAB01l
027600120416      *---
027700120416     c                   if        %Found(edTAB01l)
027800120416     c                   eval      edidsPZ = tabUNI
027900120416      *
028000120416     c                   if           §PZFTPCOD <> *zeros and
028100120416     c                                §PZFTPCOD <> *blank
028200120416     c                   move      *all'0'       CLIENTE
028300120416     c                   move      §PZFTPCOD     CLIENTE
028400120416     c                   end
028500120416     c                   end
028600120416      *---
028700120416     c                   end
028800120416      *
028900110525     c                   end
029000110525      **
029100110525      ** Comunque esce dal ciclo
029200110525     c                   leave
029300110525     c                   end
029400110525      **
029500110525     c     tabcod        reade     edTAB01l
029600110525     c                   endDo
029700110525     c*
029800110516     C                   ENDSR
029900110516      *
030000110516      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
030100110516      *  Scrittura dei segmenti IFCSUM S93A
030200110516      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
030300110516      *
030400110516     C     Exit_Segmenti BEGSR
030500110516      *
030600110516     c                   clear                   DsGenerica
030700110516     c                   eval      Tipo_DS    = SUMTPR
030800110516     c                   eval      DsGenerica = SUMDAT
030900110516     c                   call      'TRTCT91R1'
031000110525     c                   PARM                    Compatta_80       1
031100110525     c                   PARM                    CLIENTE           8
031200110516     c                   PARM                    Suffisso          2
031300110516     c                   PARM                    Tipo_DS           4
031400110516     c                   PARM                    DsGenerica     1950
031500110516     C                   PARM                    Numero_Righe      5 0
031600110617     C                   PARM                    Progressivo      10
031700110621     c                   PARM                    esito             1
031800110516      *
031900110516     C                   ENDSR
032000110516      *
032100110516      * ?================================================================== */
032200110516      *?      X non bloccare in nessun caso il traduttore CLIENTI
032300110516      * ?================================================================== */
032400110516     C     *pssr         BEGSR
032500110516     C
032600110516     C                   eval      esito = '2'
032700110516???? C                   EVAL      Oggetto ='Problemi DOWNLD MANIFEST EDI !!'
032800110707     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
032900110516     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
033000110707     C                             MANIFEST NON tradotto x invio DOWNLD. -
033100110516     C                             Controllare il Download del Cliente.'
033200110707     C                   EVAL      Messaggio = %trim(Messaggio) +
033300110707     C                                         %editc(§PTksc:'X')
033400110516     c                   exsr      Invio_Mail
033500110516     C
033600110516     C                   ENDSR
033700110516     c*==================================================================*
033800110516      * Manda un Msg x E-mail
033900110516     c*==================================================================*
034000110516     c     Invio_Mail    begsr
034100110516      *
034200110516???? C                   EVAL      Oggetto ='Problemi DOWNLD MANIFEST EDI !!'
034300110707     C                   EVAL      Oggetto = %trim(Oggetto) + %editc(§PTksc:'X')
034400110516     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
034500110516     C                             MANIFEST NON tradotto correttamente. -
034600110516     C                             Controllare il Download del Cliente.'
034700110707     C                   EVAL      Messaggio = %trim(Messaggio) +
034800110707     C                                         %editc(§PTksc:'X')
034900110516      *
035000110516     c                   exsr      SEND_MSG_EDP
035100110516      *
035200110516     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
035300110516     C* Inizializzo variabili
035400110516     C                   movel     *blanks       wrkEml          253
035500110516     C                   movel     *blanks       wrkMsg         5000
035600110516     C                   movel     *blanks       wrkOgg           44
035700110516      *
035800110516     C* Valorizzo i campi della e-m@ail - indirizzo
035900110516     C*******            eval      wrkEml = 'Andrea.Bertocchi@Bartolini.it'
036000110516     C                   eval      wrkEml = CED_BART
036100110516     C                   eval      wrkOgg = Oggetto
036200110516     C                   eval      wrkMsg = Messaggio
036300110516      *
036400110516     C                   call(e)   'TRTCT00R2'
036500110516     C                   parm                    wrkEml
036600110516     C                   parm                    wrkOgg
036700110516     C                   parm                    wrkMsg
036800110516     C*
036900110516     C*
037000110516     C                   ENDSR
037100110516     ***********************************************************************
037200110516     ** Send Message (QEZSNDMG) API
037300110516     ***********************************************************************
037400110516     C     CalQEZSNDMG   BEGSR
037500110516      *
037600110516     ** Invio messaggio all'utente.
037700110516     C                   EVAL      SndMg03 = msgDSP
037800110516     C*******            EVAL      SndMg05(Jx) = 'EDPAB     '
037900110516     C                   EVAL      SndMg05(Jx) = CED_BART
038000110516     C                   EVAL      SndMg06 = Jx
038100110516     C                   CLEAR                   QUSEC
038200110516     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038300110516
038400110516     C                   CALL      'QEZSNDMG'
038500110516     C                   PARM                    SndMg01
038600110516     C                   PARM                    SndMg02
038700110516     C                   PARM                    SndMg03
038800110516     C                   PARM                    SndMg04
038900110516     C                   PARM                    SndMg05
039000110516     C                   PARM                    SndMg06
039100110516     C                   PARM                    SndMg07
039200110516     C                   PARM                    SndMg08
039300110516     C                   PARM                    Qusec
039400110516     C                   PARM                    SndMg10
039500110516     C                   PARM                    SndMg11
039600110516     C                   PARM                    SndMg12
039700110516      *
039800110516     C                   ENDSR
039900110516     c*==================================================================*
040000110516      * Manda un Msg in Posta AS Sede a EDP*
040100110516     c*==================================================================*
040200110516     c     SEND_MSG_EDP  begsr
040300110516      *
040400110516      * INVIA MESSAGGIO ad un Utente --> EDPAB
040500110516      *   Lo manda una sola volta per lavoro
040600110516     c                   z-add     1             Jx
040700110516     C                   exsr      CalQEZSNDMG
040800110516      *
040900110516     c                   endsr
041000110516     c*
041100110516
041200110516** ======== SCHIERA: CA   (CARATTERI ALFANUMERICI)         ================
041300110516ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqsrtuvwxyz AAAAAAAAAAAAAAA 1
041400110516** ======== SCHIERA: CN   (CARATTERI NUMERICI)             ================
041500110516987654321098765432109876540123456789987654321098765432109876540999999999999999 1
