000100110713     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS')
000200090211     h DATEDIT(*DMY) DECEDIT(',')
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h* Description: ORM Immissione - Controlli parte 2
000500000000     h* Richiamato dal TIO101S su AS777 con OPCODE = CHKPARTE2
000600000000     f*--------------------------------------------------------------------------------------------*
000700000000     f* Data base
000800000000     f*--------------------------------------------------------------------------------------------*
000900000000     f*---
001000000000     f* Tabelle
001100000000     f*---
001200000000     ftabel00f  IF   E           K DISK    USROPN
001300000000     f*---
001400000000     f* Tabelle -NEW-
001500000000     f*---
001600000000     ftntbe01l  IF   E           K DISK    USROPN
001700000000     ftntbe02l  IF   E           K DISK    USROPN
001800000000     f                                     RENAME(tntbe000:tntbe2)
001900000000     f*---
002000000000     f* Organigramma
002100000000     f*---
002200000000     fazorg01l  IF   E           K DISK    USROPN
002300000000     d*--------------------------------------------------------------------------------------------*
002400000000     d* Data structure
002500000000     d*--------------------------------------------------------------------------------------------*
002600000000     d*---
002700000000     d* Costanti
002800000000     d*---
002900000000     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003000000000     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003100000000     d $Dim            C                   CONST(200)                           *n° clienti da memo
003200120829     D IRLANDA         C                   'IRL'
003300120829     D EIRE            C                   'EIRE'
003400120829
003500060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
003600060612     D  msgId                         7A   CONST
003700060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003800060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003900060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
004000060612     D                                     VARYING
004100060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
004200060612     D                                     VARYING
004300060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
004400060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
004500060612
004600060605     d*---
004700000000     d* Schiere a tempo di compilazione
004800000000     d*---
004900011023     d msg             S             80    DIM(40) CTDATA PERRCD(1)             *messaggi di uscita
005000000000     d*---
005100000000     d* Variabili riferite al data base
005200000000     d*---
005300000000     d kblkut          S                   LIKE(tblkut)                         *lettura tabel00f
005400000000     d kblcod          S                   LIKE(tblcod)
005500090210     d kblkey          S                   LIKE(tblkey)
005600000000     d kbecod          S                   LIKE(tbecod)                         *lettura tntbe01l
005700000000     d kbeke1          S                   LIKE(tbeke1)
005800000000     d kbelin          S                   LIKE(tbelin)
005900000000     d kbesif          S                   LIKE(tbesif)
006000000000     d deplna          S                   LIKE(o95lna)                         *depositi
006100000000     d deplad          S                   LIKE(o95lna)
006200000000     d NInpo107Dar     S                   LIKE(Inpo107Dar)                     *cambi"normalizzati"
006300000000     d NInpo107Orr     S                   LIKE(Inpo107Orr)
006400000000     d NInpo107AD1     S                   LIKE(Inpo107AD1)
006500000000     d NInpo107AA1     S                   LIKE(Inpo107AA1)
006600000000     d NInpo107AD2     S                   LIKE(Inpo107AD2)
006700000000     d NInpo107AA2     S                   LIKE(Inpo107AA2)
006800000000     d NInpo107Rer     S                   LIKE(Inpo107Rer)
006900000000     d NInpo107Ter     S                   LIKE(Inpo107Ter)
007000000000     d NInpo107Rsc     S                   LIKE(Inpo107Rsc)
007100000000     d NInpo107Inc     S                   LIKE(Inpo107Inc)
007200000000     d NInpo107Cac     S                   LIKE(Inpo107Cac)
007300000000     d NInpo107Loc     S                   LIKE(Inpo107Loc)
007400000000     d NInpo107Prc     S                   LIKE(Inpo107Prc)
007500000000     d NInpo107Nac     S                   LIKE(Inpo107Nac)
007600000000     d NInpo107Nam     S                   LIKE(Inpo107Nam)
007700000000     d*---
007800000000     d* Variabili di controlli
007900000000     d*---
008000000000     d $e              S              2  0                                      *indice errori Out
008100000000     d $err            S              1                                         *controllore errore
008200000000     d $oknum          S              1                                         *validità numerica
008300000000     d*---
008400000000     d* Variabili NON riferite al data base
008500000000     d*---
008600000000     d i               S              3  0                                      *indici
008700000000     d ii              S              3  0
008800000000     d n14             S             14  0                                      *Numerico 14
008900000000     d n2              S              2  0                                      *Numerico 2
009000000000     d n3              S              3  0                                      *Numerico 3
009100000000     d n8              S              8  0                                      *Numerico 8
009200000000     d n71             S              7  1                                      *Numerico 7,1
009300000000     d a1              S              1                                         *Alfabetico 1
009400000000     d a3              S              3                                         *Alfabetico 3
009500000000     d a7              S              7                                         *Alfabetico 7
009600000000     d a5              S              5                                         *Alfabetico 5
009700000000     d datcor          S              8  0                                      *data corrente a/m/g
009800000000     d oracor          S              6  0                                      *ora corrente
009900000000     d dardep          S              8  0                                      *dep.data pr.merce
010000000000     d werr            S              1                                         *errore generico
010100010919     d wimpmsg         S              1                                         *msg da impostare
010200010919     d wmsg            S             80                                         *msg impostato
010300000000     d wpgm            S             20                                         *programma
010400000000     d wdat            S              8  0                                      *data (aaaammgg)
010500000000     d wdate           S             10                                         *data (gg/mm/aaaa)
010600000000     d wora            S              4  0                                      *ora  (hhmm)
010700000000     d worae           S              5                                         *ora  (hh:mm)
010800060605     d*---
010900000000     d* Schiere
011000000000     d*---
011100000000     d SErr            S              1    DIM(10)                              *errori          Out
011200000000     d SMsg            S             80    DIM(10)                              *messaggi errori Out
011300000000      * tabella filiali
011400000000     d SOrg            S              3  0 DIM(300)                             *filiale
011500000000     d SOdes           S             30    DIM(300)                             *descrizione filiale
011600000000     d SOfl1           S              1    DIM(300)                             *flag italia/estero
011700020702     d SOpt            S              3    DIM(300)                             *flag poste/no poste
011800020702     d SOdpd           S              3    DIM(300)                             *flag DPD/no DPD
011900000000      * tabella provincie
012000000000     d SPrv            S              2    DIM(200)                             *provincia
012100000000     d SPDes           S             20    DIM(200)                             *descriz.provincia
012200000000      * tabella nazioni
012300011218     d SNaz            S              3    DIM(500)                             *nazione
012400011218     d SNDes           S             25    DIM(500)                             *descrizione nazione
012500011218     d SNLad           S              3  0 DIM(500)                             *l.arrivo DPD
012600000000     d*---
012700000000     d* Parametri I/O
012800000000     d*---
012900000000     d tio107IDs     E Ds                  PREFIX(Inp)                          *Input
013000000000     d tio107ODs     E Ds                  PREFIX(Out)                          *Output
013100000000     d*---
013200000000     d* Ds
013300000000     d*---
013400110713     D kpjba         E DS                  INZ
013500000000     d* controllo data
013600000000     d wlbda8          DS                  INZ                                  *controlla data
013700000000     d  g08dat                        8  0                                       -data dritta
013800000000     d  g08inv                        8  0                                       -data invertita
013900000000     d  g08err                        1                                          -errore
014000000000     d  g08tgi                        5  0                                       -giorni fra date
014100000000      * scomposizione data generica
014200000000     d dsdat           DS                  INZ                                  *data
014300000000     d  dsann                         4  0                                       -anno
014400000000     d  dsmes                         2  0                                       -mese
014500000000     d  dsgio                         2  0                                       -giorno
014600000000      * scomposizione data generica (anno + mese/giorno)
014700000000     d                 DS                  INZ                                  *data aa + mm/gg
014800000000     d  dsannB                 1      4  0                                       -anno
014900000000     d  dsmgsB                 5      8  0                                       -mese/giorno
015000000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
015100000000      * composizione data generica editata
015200000000     d dsdate          DS                  INZ                                  *data
015300000000     d  dsgioe                        2  0                                       -giorno
015400000000     d  dsvd1                         1                                          -'/'
015500000000     d  dsmese                        2  0                                       -mese
015600000000     d  dsvd2                         1                                          -'/'
015700000000     d  dsanne                        4  0                                       -anno
015800000000      * scomposizione ora generica
015900000000     d dsora           DS                  INZ                                  *ora
016000000000     d  dshh                          2  0                                       -ore
016100000000     d  dsmm                          2  0                                       -minuti
016200000000      * composizione ora generica editata
016300000000     d dsorae          DS                  INZ                                  *ora
016400000000     d  dshhe                         2  0                                       -ore
016500000000     d  dsvo1                         1                                          -':'
016600000000     d  dsmme                         2  0                                       -minuti
016700000000     d* controlla cap
016800000000     d tisi95ds      E DS                                                       *parametri chiamata
016900081118     d tisi97ds      E DS                  INZ
017000081118     D fnlv12ds      E DS                  INZ
017100110713     D fior95ds      E DS                  INZ
017200110713     D  ior95tla     E                     INZ('C')
017300000000      * lettura campo orgde3 di AZORG00F
017400000000     d og143         E DS
017500000000      * DS Provincie
017600000000     d dspr          E DS
017700000000      * DS Nazioni
017800000000     d ds15          E DS
017900020702      * DS lettura tabella "NTW"
018000020702     d DNTW          E DS
018100090210     d ds3idp        E DS                  QUALIFIED
018200090210     d                                     INZ
018300081118
018400041125     D Esito           S              5I 0
018500041125     D Msg708          S             80
018600110713
018700110713     D/COPY GAITRASRC/SRCPROTOPR,TIBS0800R
018800110713
018900000000     c*--------------------------------------------------------------------------------------------*
019000000000     c* Main lines
019100000000     c*--------------------------------------------------------------------------------------------*
019200000000     c*
019300000000     c* operazioni iniziali -da fare sempre-
019400000000     c                   EXSR      rutinz
019500011023     c*
019600011023     c* controlla input parte 1
019700011023     c                   EXSR      ChkParte1
019800000000     c*
019900000000     c* controlla la data pronta merce
020000041125     c*                  EXSR      ChkInpDar
020100000000     c*
020200000000     c* controlla l' ora pronta merce
020300041125     c*                  EXSR      ChkInpOrr
020400041125     c*
020500041125     c* Controlla data e ora pronta merce.
020600041125     c                   EXSR      ChkDaROrR
020700000000     c*
020800000000     c* controlla gli orari di apertura UNO
020900000000     c                   EXSR      ChkInpOA1
021000000000     c*
021100000000     c* controlla gli orari di apertura DUE
021200000000     c                   EXSR      ChkInpOA2
021300000000     c*
021400000000     c* controlla gli orari di apertura UNO e DUE
021500000000     c                   EXSR      ChkInpOA1OA2
021600000000     c*
021700000000     c* controlla la quantità merce
021800000000     c                   EXSR      ChkInpQta
021900000000     c*
022000000000     c* controlla la qualità merce
022100000000     c                   EXSR      ChkInpQlt
022200000000     c*
022300000000     c* controlla la qualità merce
022400000000     c                   EXSR      ChkInpQlt
022500000000     c*
022600000000     c* controlla se ritiro singolo / multiplo
022700000000     c                   EXSR      ChkInpUPD
022800000000     c*
022900000000     c* controlla la persona da contattare
023000000000     c                   EXSR      ChkInpRer
023100000000     c*
023200000000     c* controlla il destinatario
023300000000     c                   EXSR      ChkInpDes
023400020517     c*
023500000000     c* controlla il fermo deposito
023600000000     c                   EXSR      ChkInpFfd
023700000000     c*
023800000000     c* controlla la scelta del network estero
023900000000     c                   EXSR      ChkInpSse
024000000000     c*
024100000000     c* imposta i campi della DS di Output O eventuali errori
024200000000if  1c                   IF        $err = '0'                                   *NO errori
024300000000     c                   EXSR      ImpDsOut
024400000000X   1C                   ELSE                                                   *SI errori
024500000000     c                   MOVEA     SErr          Outo107Err                     *codice
024600000000     c                   MOVEA     SMsg          Outo107Msg                     *messaggio
024700000000e   1c                   ENDIF
024800010919     c*
024900010919     c* operazioni finali
025000010919     c                   EXSR      rutfin
025100000000     c*
025200000000     C                   RETURN
025300011023     c*--------------------------------------------------------------------------------------------*
025400011023     c* controlla input parte 1
025500011023     c*--------------------------------------------------------------------------------------------*
025600011023     c     ChkParte1     BEGSR
025700011023     c*---
025800011023     c* Può capitare che le persone si addormentino sopra le pagine e in tal modo i dati immessi,
025900011023     c* anche se controllati e corretti, non rimangano memorizzate perchè è passato troppo tempo.
026000011023     c* Occorre, quando si passa da una videata all'altra, RIcontrollare i dati immessi prima,
026100011023     c* testandone solamente l'obbligatorietà (se ci sono, sono corretti).
026200011023     c*---
026300011023     c*
026400011023     c* reimposta le variabili di lavoro
026500011023     c                   MOVEL     '0'           werr                           *NO errore
026600011023     c*
026700011023     c* Cliente ritiro / Nominativo ritiro
026800011023if  1c                   IF        InpO107Cro <> *blanks OR
026900011023     c                             InpO107Rsr <> *blanks
027000011023x   1c                   ELSE
027100011023     c                   MOVEL     '1'           werr                           *errore Output
027200011023e   1c                   ENDIF
027300011023     c* Pagamemto
027400011023if  1c                   IF        InpO107Pag <> *blanks
027500011023x   1c                   ELSE
027600011023     c                   MOVEL     '1'           werr                           *errore Output
027700011023e   1c                   ENDIF
027800011023     c*
027900011023     c* se errore, memorizza il codice errore nella schiera errori
028000011023if  1c                   IF        werr = '1'
028100011023if  2c                   IF        $e < 10                                      *non + di 10 errori
028200011023     c                   ADD       1             $e
028300011023     c                   MOVEL     '£'           Serr($e)                       *aggiunge errore
028400060605     c*                  MOVEL     msg(40)       Smsg($e)                       *messaggio
028500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0587'
028600060609     C                             :inpO107Lang)
028700060609     c                   MOVEL     '1'           $err                           *controllore errore
028800011023e   2c                   ENDIF
028900011023e   1c                   ENDIF
029000011023     c*
029100011023     c                   ENDSR
029200000000     c*--------------------------------------------------------------------------------------------*
029300000000     c* controlla la data pronta merce
029400000000     c*--------------------------------------------------------------------------------------------*
029500000000     c     ChkInpDar     BEGSR
029600000000     c*
029700000000     c* reimposta le variabili di lavoro
029800000000     c                   MOVEL     '0'           werr                           *NO errore
029900010919     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
030000010919     c                   CLEAR                   wmsg                           *msg impostato
030100000000     c                   CLEAR                   NInpo107Dar                    *campo"normalizzato"
030200000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
030300000000     c                   CLEAR                   dardep                         *depositi
030400000000     c*
030500000000     c* controlla se la data pronta merce è tutta un numero
030600000000do  1c                   DO
030700000000     c                   SETOFF                                       98
030800000000     c                   TESTN                   Inpo107Dar           98
030900000000if  2c                   IF        *in98                                        *da TESTN è numero
031000000000     c                   MOVE      Inpo107Dar    a1                             *ultimo carattere
031100000000if  3c                   IF        a1 >= '0'                                    *NON trovata lettera
031200000000     c                   MOVEL     'S'           $OkNum                         *valido
031300000000e   3c                   ENDIF
031400000000e   2c                   ENDIF
031500000000if  2c                   IF        $OkNum = 'N'                                 *NON valido
031600000000     c                   MOVEL     '1'           werr                           *errore Output
031700000000     c                   LEAVE
031800000000e   2c                   ENDIF
031900000000     c*
032000000000     c* sposta il campo in quello "normalizzato"
032100000000     c                   MOVEL     Inpo107Dar    NInpo107Dar                    *campo"normalizzato"
032200000000     c*
032300000000     c* controlla validità
032400000000     C                   MOVEL     NInpo107Dar   n8                             *data (8) in g/m/a
032500000000     C                   Z-ADD     n8            g08dat
032600000000     C                   Z-ADD     *zeros        g08inv
032700000000     C                   MOVEL     '0'           g08err
032800000000     c                   CALL      'XSRDA8'
032900000000     c                   PARM                    wlbda8
033000000000     c                   Z-ADD     g08inv        dardep
033100000000if  2c                   IF        g08err = '1'                                 *data errata
033200000000     c                   MOVEL     '1'           werr                           *errore Output
033300000000     c                   LEAVE
033400000000e   2c                   ENDIF
033500010919     c*
033600010919     c* reperimento data + ora corrente
033700010919     C                   TIME                    n14                            *reperimento ora
033800010919     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
033900010919     C                   MOVE      n14           n8                             *data (8) in g/m/a
034000010919     C                   Z-ADD     n8            g08dat
034100010919     C                   Z-ADD     *zeros        g08inv
034200010919     C                   MOVEL     '0'           g08err
034300010919     c                   CALL      'XSRDA8'
034400010919     c                   PARM                    wlbda8
034500010919     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
034600000000     c*
034700000000     c* la data non può essere di un giorno PRIMA di oggi
034800000000if  2c                   IF        dardep < datcor
034900000000     c                   MOVEL     '1'           werr                           *errore Output
035000000000     c                   LEAVE
035100000000e   2c                   ENDIF
035200010919     c*
035300010919     c* se il tempo di ritiro del CAP di ritiro è -->
035400041124     c*  - "01" se l'ora corrente è entro le 12.30 allora la data ritiro può essere oggi
035500041124     c*         se l'ora corrente è dopo  le 12.30 allora la data ritiro NON può essere oggi
035600010919     c*  - "02" la data ritiro NON può essere oggi
035700010919if  2c                   IF        dardep = datcor
035800010919     C                   CLEAR                   tisi95ds
035900010919     C                   MOVEL     ' '           I95tla
036000010919     C                   MOVEL     '3'           I95tcn
036100010919     C                   MOVEL     InpO107Nar    I95nar
036200010919     C                   MOVEL     InpO107Car    I95cap
036300010919     C                   MOVEL     InpO107Lor    I95loc
036400010919     C                   MOVEL     InpO107Prr    I95prv
036500010919     C                   Z-ADD     datcor        I95dat
036600120829     C                   IF        i95nar = IRLANDA
036700120829     C                   EVAL      i95cap = EIRE
036800120829     C                   ENDIF
036900120829     C                   CALL      'TISI95R'                                    *reperimento tempo
037000010919     C                   PARM                    tisi95ds
037100000000     c*
037200010919if  3c                   IF        O95gf2 = '01' AND
037300041124     c                             oracor > 123000 OR
037400010919     c                             O95gf2 = '02'
037500010919     c                   MOVEL     '1'           werr                           *errore Output
037600010919     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
037700060605     c*                  MOVEL     msg(27)       wmsg                           *messaggio
037800060609     C                   EVAL      wmsg = rtvMsgLang('TIS0301'
037900060609     C                             :inpO107Lang)
038000010919     c                   LEAVE
038100010919e   3c                   ENDIF
038200010919e   2c                   ENDIF
038300010919     c*
038400000000e   1c                   ENDDO
038500000000     c*
038600000000     c* se errore, memorizza il codice errore nella schiera errori
038700000000if  1c                   IF        werr = '1'
038800000000if  2c                   IF        $e < 10                                      *non + di 10 errori
038900000000     c                   ADD       1             $e
039000000000     c                   MOVEL     '1'           Serr($e)                       *aggiunge errore
039100010919if  3c                   IF        wimpmsg = '0'                                *msg da impostare
039200060605     c*                  MOVEL     msg(1)        Smsg($e)                       *messaggio
039300060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0116'
039400060609     C                             :inpO107Lang)
039500010919x   3c                   ELSE                                                   *msg già impostato
039600010919     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
039700010919e   3c                   ENDIF
039800000000     c                   MOVEL     '1'           $err                           *controllore errore
039900010919e   2c                   ENDIF
040000000000e   1c                   ENDIF
040100000000     c*
040200000000     c                   ENDSR
040300000000     c*--------------------------------------------------------------------------------------------*
040400000000     c* controlla l' ora pronta merce
040500000000     c*--------------------------------------------------------------------------------------------*
040600000000     c     ChkInpOrr     BEGSR
040700000000     c*
040800000000     c* reimposta le variabili di lavoro
040900000000     c                   MOVEL     '0'           werr                           *NO errore
041000000000     c                   CLEAR                   NInpo107Orr                    *campo"normalizzato"
041100000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
041200000000     c*
041300000000     c* controlla se l' ora pronta merce è tutta un numero
041400000000do  1c                   DO
041500000000     c                   SETOFF                                       98
041600000000     c                   TESTN                   Inpo107Orr           98
041700000000if  2c                   IF        *in98                                        *da TESTN è numero
041800000000     c                   MOVE      Inpo107Orr    a1                             *ultimo carattere
041900000000if  3c                   IF        a1 >= '0'                                    *NON trovata lettera
042000000000     c                   MOVEL     'S'           $OkNum                         *valido
042100000000e   3c                   ENDIF
042200000000e   2c                   ENDIF
042300000000if  2c                   IF        $OkNum = 'N'                                 *NON valido
042400000000     c                   MOVEL     '1'           werr                           *errore Output
042500000000     c                   LEAVE
042600000000e   2c                   ENDIF
042700000000     c*
042800000000     c* sposta il campo in quello "normalizzato"
042900000000     c                   MOVEL     Inpo107Orr    NInpo107Orr                    *campo"normalizzato"
043000000000     c*
043100000000     c* controlla validità
043200000000     C                   MOVEL     NInpo107Orr   n2                             *hh
043300000000if  2c                   IF        n2 >=  0 AND
043400000000     c                             n2 <= 23
043500000000x   2c                   ELSE
043600000000     c                   MOVEL     '1'           werr                           *errore Output
043700000000     c                   LEAVE
043800000000e   2c                   ENDIF
043900000000     c*
044000000000     C                   MOVE      NInpo107Orr   n2                             *mm
044100000000if  2c                   IF        n2 >=  0 AND
044200000000     c                             n2 <= 59
044300000000x   2c                   ELSE
044400000000     c                   MOVEL     '1'           werr                           *errore Output
044500000000     c                   LEAVE
044600000000e   2c                   ENDIF
044700000000     c*
044800000000e   1c                   ENDDO
044900000000     c*
045000000000     c* se errore, memorizza il codice errore nella schiera errori
045100000000if  1c                   IF        werr = '1'
045200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
045300000000     c                   ADD       1             $e
045400000000     c                   MOVEL     '2'           Serr($e)                       *aggiunge errore
045500060605     c*                  MOVEL     msg(2)        Smsg($e)                       *messaggio
045600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0423'
045700060609     C                             :inpO107Lang)
045800000000     c                   MOVEL     '1'           $err                           *controllore errore
045900000000e   2c                   ENDIF
046000000000e   1c                   ENDIF
046100000000     c*
046200000000     c                   ENDSR
046300041125
046400041125     ***********************************************************************************************
046500041125     **
046600041125     ** Controllo data e ora pronta merce.
046700041125     **
046800041125     ***********************************************************************************************
046900041125     C     ChkDaROrR     BEGSR
047000041125
047100041125     C                   CALL(E)   'TIO708R'
047200041125     C                   PARM                    InpO107RQSCID
047300041125     C                   PARM                    InpO107KSU
047400041125     C                   PARM                    InpO107CRO
047500041125     C                   PARM                    InpO107CAR
047600041125     C                   PARM                    InpO107LOR
047700041125     C                   PARM                    InpO107PRR
047800041125     C                   PARM                    InpO107NAR
047900041125     C                   PARM                    InpO107DAR
048000041125     C                   PARM                    InpO107ORR
048100041125     C                   PARM      0             Esito
048200041125     C                   PARM      *BLANK        Msg708
048300060605     C                   PARM                    inpO107Lang
048400041125
048500041125     ** se errore, memorizza il codice errore nella schiera errori
048600041125     C                   IF        Esito < 0
048700041125if  2c                   IF        $e < 10                                      *non + di 10 errori
048800041125     c                   ADD       1             $e
048900041125     c                   MOVEL     '2'           Serr($e)                       *aggiunge errore
049000041125     c                   MOVEL     Msg708        Smsg($e)                       *messaggio
049100041125     c                   MOVEL     '1'           $err                           *controllore errore
049200041125e   2c                   ENDIF
049300041125e   1c                   ENDIF
049400041125
049500041125     C                   ENDSR
049600041125
049700000000     c*--------------------------------------------------------------------------------------------*
049800000000     c* controlla gli orari di apertura UNO
049900000000     c*--------------------------------------------------------------------------------------------*
050000000000     c     ChkInpOA1     BEGSR
050100000000     c*
050200000000     c* reimposta le variabili di lavoro
050300000000     c                   MOVEL     '0'           werr                           *NO errore
050400000000     c                   CLEAR                   NInpo107AD1                    *campo"normalizzato"
050500000000     c                   CLEAR                   NInpo107AA1
050600000000     c*
050700000000     c* controlla i dati solo se immessi
050800000000if  1c                   IF        Inpo107AD1 <> *blanks OR
050900000000     c                             Inpo107AA1 <> *blanks
051000000000do  2c                   DO
051100000000     c*---
051200000000     c* Da
051300000000     c*---
051400000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
051500000000     c                   SETOFF                                       98
051600000000     c                   TESTN                   Inpo107AD1           98
051700000000if  3c                   IF        *in98                                        *da TESTN è numero
051800000000     c                   MOVE      Inpo107AD1    a1                             *ultimo carattere
051900000000if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
052000000000     c                   MOVEL     'S'           $OkNum                         *valido
052100000000e   4c                   ENDIF
052200000000e   3c                   ENDIF
052300000000if  3c                   IF        $OkNum = 'N'                                 *NON valido
052400000000     c                   MOVEL     '1'           werr                           *errore Output
052500000000     c                   LEAVE
052600000000e   3c                   ENDIF
052700000000     c*
052800000000     c* sposta il campo in quello "normalizzato"
052900000000     c                   MOVEL     Inpo107AD1    NInpo107AD1                    *campo"normalizzato"
053000000000     c*
053100000000     c* controlla validità
053200000000     C                   MOVEL     NInpo107AD1   n2                             *hh
053300000000if  3c                   IF        n2 >=  0 AND
053400000000     c                             n2 <= 23
053500000000x   3c                   ELSE
053600000000     c                   MOVEL     '1'           werr                           *errore Output
053700000000     c                   LEAVE
053800000000e   3c                   ENDIF
053900000000     c*
054000000000     C                   MOVE      NInpo107AD1   n2                             *mm
054100000000if  3c                   IF        n2 >=  0 AND
054200000000     c                             n2 <= 59
054300000000x   3c                   ELSE
054400000000     c                   MOVEL     '1'           werr                           *errore Output
054500000000     c                   LEAVE
054600000000e   3c                   ENDIF
054700000000     c*---
054800000000     c* A
054900000000     c*---
055000000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
055100000000     c                   SETOFF                                       98
055200000000     c                   TESTN                   Inpo107AA1           98
055300000000if  3c                   IF        *in98                                        *da TESTN è numero
055400000000     c                   MOVE      Inpo107AA1    a1                             *ultimo carattere
055500000000if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
055600000000     c                   MOVEL     'S'           $OkNum                         *valido
055700000000e   4c                   ENDIF
055800000000e   3c                   ENDIF
055900000000if  3c                   IF        $OkNum = 'N'                                 *NON valido
056000000000     c                   MOVEL     '1'           werr                           *errore Output
056100000000     c                   LEAVE
056200000000e   3c                   ENDIF
056300000000     c*
056400000000     c* sposta il campo in quello "normalizzato"
056500000000     c                   MOVEL     Inpo107AA1    NInpo107AA1                    *campo"normalizzato"
056600000000     c*
056700000000     c* controlla validità
056800000000     C                   MOVEL     NInpo107AA1   n2                             *hh
056900000000if  3c                   IF        n2 >=  0 AND
057000000000     c                             n2 <= 23
057100000000x   3c                   ELSE
057200000000     c                   MOVEL     '1'           werr                           *errore Output
057300000000     c                   LEAVE
057400000000e   3c                   ENDIF
057500000000     c*
057600000000     C                   MOVE      NInpo107AA1   n2                             *mm
057700000000if  3c                   IF        n2 >=  0 AND
057800000000     c                             n2 <= 59
057900000000x   3c                   ELSE
058000000000     c                   MOVEL     '1'           werr                           *errore Output
058100000000     c                   LEAVE
058200000000e   3c                   ENDIF
058300000000     c*---
058400000000     c* Da + A
058500000000     c*---
058600000000     c* l'ora A deve essere maggiore dell'ora DA
058700000000if  3c                   IF        NInpo107AA1 < NInpo107AD1
058800000000     c                   MOVEL     '1'           werr                           *errore Output
058900000000     c                   LEAVE
059000000000e   3c                   ENDIF
059100000000     c*
059200000000e   2c                   ENDDO
059300000000e   1c                   ENDIF
059400000000     c*
059500000000     c* se errore, memorizza il codice errore nella schiera errori
059600000000if  1c                   IF        werr = '1'
059700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
059800000000     c                   ADD       1             $e
059900000000     c                   MOVEL     '3'           Serr($e)                       *aggiunge errore
060000060605     c*                  MOVEL     msg(3)        Smsg($e)                       *messaggio
060100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0457'
060200060609     C                             :inpO107Lang)
060300000000     c                   MOVEL     '1'           $err                           *controllore errore
060400000000e   2c                   ENDIF
060500000000e   1c                   ENDIF
060600000000     c*
060700000000     c                   ENDSR
060800000000     c*--------------------------------------------------------------------------------------------*
060900000000     c* controlla gli orari di apertura DUE
061000000000     c*--------------------------------------------------------------------------------------------*
061100000000     c     ChkInpOA2     BEGSR
061200000000     c*
061300000000     c* reimposta le variabili di lavoro
061400000000     c                   MOVEL     '0'           werr                           *NO errore
061500000000     c                   CLEAR                   NInpo107AD2                    *campo"normalizzato"
061600000000     c                   CLEAR                   NInpo107AA2
061700000000     c*
061800000000     c* controlla i dati solo se immessi
061900000000if  1c                   IF        Inpo107AD2 <> *blanks OR
062000000000     c                             Inpo107AA2 <> *blanks
062100000000do  2c                   DO
062200000000     c*---
062300000000     c* Da
062400000000     c*---
062500000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
062600000000     c                   SETOFF                                       98
062700000000     c                   TESTN                   Inpo107AD2           98
062800000000if  3c                   IF        *in98                                        *da TESTN è numero
062900000000     c                   MOVE      Inpo107AD2    a1                             *ultimo carattere
063000000000if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
063100000000     c                   MOVEL     'S'           $OkNum                         *valido
063200000000e   4c                   ENDIF
063300000000e   3c                   ENDIF
063400000000if  3c                   IF        $OkNum = 'N'                                 *NON valido
063500000000     c                   MOVEL     '1'           werr                           *errore Output
063600000000     c                   LEAVE
063700000000e   3c                   ENDIF
063800000000     c*
063900000000     c* sposta il campo in quello "normalizzato"
064000000000     c                   MOVEL     Inpo107AD2    NInpo107AD2                    *campo"normalizzato"
064100000000     c*
064200000000     c* controlla validità
064300000000     C                   MOVEL     NInpo107AD2   n2                             *hh
064400000000if  3c                   IF        n2 >=  0 AND
064500000000     c                             n2 <= 23
064600000000x   3c                   ELSE
064700000000     c                   MOVEL     '1'           werr                           *errore Output
064800000000     c                   LEAVE
064900000000e   3c                   ENDIF
065000000000     c*
065100000000     C                   MOVE      NInpo107AD2   n2                             *mm
065200000000if  3c                   IF        n2 >=  0 AND
065300000000     c                             n2 <= 59
065400000000x   3c                   ELSE
065500000000     c                   MOVEL     '1'           werr                           *errore Output
065600000000     c                   LEAVE
065700000000e   3c                   ENDIF
065800000000     c*---
065900000000     c* A
066000000000     c*---
066100000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
066200000000     c                   SETOFF                                       98
066300000000     c                   TESTN                   Inpo107AA2           98
066400000000if  3c                   IF        *in98                                        *da TESTN è numero
066500000000     c                   MOVE      Inpo107AA2    a1                             *ultimo carattere
066600000000if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
066700000000     c                   MOVEL     'S'           $OkNum                         *valido
066800000000e   4c                   ENDIF
066900000000e   3c                   ENDIF
067000000000if  3c                   IF        $OkNum = 'N'                                 *NON valido
067100000000     c                   MOVEL     '1'           werr                           *errore Output
067200000000     c                   LEAVE
067300000000e   3c                   ENDIF
067400000000     c*
067500000000     c* sposta il campo in quello "normalizzato"
067600000000     c                   MOVEL     Inpo107AA2    NInpo107AA2                    *campo"normalizzato"
067700000000     c*
067800000000     c* controlla validità
067900000000     C                   MOVEL     NInpo107AA2   n2                             *hh
068000000000if  3c                   IF        n2 >=  0 AND
068100000000     c                             n2 <= 23
068200000000x   3c                   ELSE
068300000000     c                   MOVEL     '1'           werr                           *errore Output
068400000000     c                   LEAVE
068500000000e   3c                   ENDIF
068600000000     c*
068700000000     C                   MOVE      NInpo107AA2   n2                             *mm
068800000000if  3c                   IF        n2 >=  0 AND
068900000000     c                             n2 <= 59
069000000000x   3c                   ELSE
069100000000     c                   MOVEL     '1'           werr                           *errore Output
069200000000     c                   LEAVE
069300000000e   3c                   ENDIF
069400000000     c*---
069500000000     c* Da + A
069600000000     c*---
069700000000     c* l'ora A deve essere maggiore dell'ora DA
069800000000if  3c                   IF        NInpo107AA2 < NInpo107AD2
069900000000     c                   MOVEL     '1'           werr                           *errore Output
070000000000     c                   LEAVE
070100000000e   3c                   ENDIF
070200000000     c*
070300000000e   2c                   ENDDO
070400000000e   1c                   ENDIF
070500000000     c*
070600000000     c* se errore, memorizza il codice errore nella schiera errori
070700000000if  1c                   IF        werr = '1'
070800000000if  2c                   IF        $e < 10                                      *non + di 10 errori
070900000000     c                   ADD       1             $e
071000000000     c                   MOVEL     '4'           Serr($e)                       *aggiunge errore
071100060605     c*                  MOVEL     msg(4)        Smsg($e)                       *messaggio
071200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0541'
071300060609     C                             :inpO107Lang)
071400000000     c                   MOVEL     '1'           $err                           *controllore errore
071500000000e   2c                   ENDIF
071600000000e   1c                   ENDIF
071700000000     c*
071800000000     c                   ENDSR
071900000000     c*--------------------------------------------------------------------------------------------*
072000000000     c* controlla gli orari di apertura UNO e DUE
072100000000     c*--------------------------------------------------------------------------------------------*
072200000000     c     ChkInpOA1OA2  BEGSR
072300000000     c*
072400000000     c* reimposta le variabili di lavoro
072500000000     c                   MOVEL     '0'           werr                           *NO errore
072600000000     c*
072700000000     c* se immessi primi e secondi occorre controllarne la congruità
072800000000if  1c                   IF        Inpo107AD1 <> *blanks AND
072900000000     c                             Inpo107AD2 <> *blanks
073000000000do  2c                   DO
073100000000     c*
073200000000if  3c                   IF        NInpo107AD2 < NInpo107AA1
073300000000     c                   MOVEL     '1'           werr                           *errore Output
073400000000     c                   LEAVE
073500000000e   3c                   ENDIF
073600000000     c*
073700000000e   2c                   ENDDO
073800000000e   1c                   ENDIF
073900000000     c*
074000000000     c* se errore, memorizza il codice errore nella schiera errori
074100000000if  1c                   IF        werr = '1'
074200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
074300000000     c                   ADD       1             $e
074400000000     c                   MOVEL     '5'           Serr($e)                       *aggiunge errore
074500060605     c*                  MOVEL     msg(5)        Smsg($e)                       *messaggio
074600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0171'
074700060609     C                             :inpO107Lang)
074800000000     c                   MOVEL     '1'           $err                           *controllore errore
074900000000e   2c                   ENDIF
075000000000e   1c                   ENDIF
075100000000     c*
075200000000     c                   ENDSR
075300000000     c*--------------------------------------------------------------------------------------------*
075400000000     c* controlla la quantità merce
075500000000     c*--------------------------------------------------------------------------------------------*
075600000000     c     ChkInpQta     BEGSR
075700000000     c*
075800000000     c* reimposta le variabili di lavoro
075900000000     c                   MOVEL     '0'           werr                           *NO errore
076000000000     c                   CLEAR                   NInpo107Pkg       7 1          *campi"normalizzati"
076100000000     c                   CLEAR                   NInpo107Vlm       5 3
076200000000     c                   CLEAR                   NInpo107Bnc       5 0
076300000000     c                   CLEAR                   NInpo107Ncl       5 0
076400000000     c                   CLEAR                   NInpo107Blc       5 0
076500000000     c                   CLEAR                   NInpo107Att       5 0
076600000000     c                   CLEAR                   NInpo107Mtc       5 0
076700000000     c                   CLEAR                   a7
076800000000     c                   CLEAR                   a5
076900000000     c*---
077000000000     c* Peso
077100000000     c*---
077200000000     c*
077300000000     c* controlla il campo solo se immesso
077400000000if  1c                   IF        Inpo107Pkg <> *blanks
077500000000     c     ' ' :'0'      XLATE     Inpo107Pkg    a7
077600000000do  2c                   DO
077700000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
077800000000     c                   SETOFF                                       98
077900000000     c                   TESTN                   a7                   98
078000000000if  3c                   IF        *in98                                        *da TESTN è numero
078100000000     c                   MOVE      a7            a1                             *ultimo carattere
078200000000if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
078300000000     c                   MOVEL     'S'           $OkNum                         *valido
078400000000e   4c                   ENDIF
078500000000e   3c                   ENDIF
078600000000if  3c                   IF        $OkNum = 'N'                                 *NON valido
078700000000     c                   MOVEL     '1'           werr                           *errore Output
078800000000     c                   LEAVE
078900000000e   3c                   ENDIF
079000000000     c*
079100000000     c* sposta il campo in quello "normalizzato"
079200000000     c                   MOVEL     a7            NInpo107Pkg                    *campo"normalizzato"
079300000000e   2c                   ENDDO
079400000000e   1c                   ENDIF
079500000000     c*
079600000000     c* se errore, memorizza il codice errore nella schiera errori
079700000000if  1c                   IF        werr = '1'
079800000000if  2c                   IF        $e < 10                                      *non + di 10 errori
079900000000     c                   ADD       1             $e
080000000000     c                   MOVEL     '6'           Serr($e)                       *aggiunge errore
080100060605     c*                  MOVEL     msg(6)        Smsg($e)                       *messaggio
080200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0453'
080300060609     C                             :inpO107Lang)
080400000000     c                   MOVEL     '1'           $err                           *controllore errore
080500000000e   2c                   ENDIF
080600000000e   1c                   ENDIF
080700090210     ** Se richiesto il ritiro con DPD
080800090210     ** il peso diventa obbligatorio e deve essere inferiore al limite.
080900090210     C                   IF        wErr = '0' AND
081000090210     C                             inpo107sse = 'D' AND
081100090210     C                             ds3idp.§3iPkd > 0 AND
081200090210     C                             (NInpo107Pkg = 0 OR
081300090210     C                              NInpo107Pkg > ds3idp.§3iPkd)
081400090210     c                   IF        $e < 10                                      *non + di 10 errori
081500090210     c                   ADD       1             $e
081600090210     c                   MOVEL     '6'           Serr($e)                       *aggiunge errore
081700090211     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0694'
081800090211     C                             : inpO107Lang : %EDITC(ds3idp.§3iPkd:'3'))
081900090210     c                   MOVEL     '1'           $err                           *controllore errore
082000090210     c                   ENDIF
082100090210     C                   ENDIF
082200000000     c*---
082300000000     c* Volume
082400000000     c*---
082500000000     c*
082600000000     c* controlla il campo solo se immesso
082700000000if  1c                   IF        werr = '0'
082800000000if  2c                   IF        Inpo107Vlm <> *blanks
082900000000     c     ' ' :'0'      XLATE     Inpo107Vlm    a5
083000000000do  3c                   DO
083100000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
083200000000     c                   SETOFF                                       98
083300000000     c                   TESTN                   a5                   98
083400000000if  4c                   IF        *in98                                        *da TESTN è numero
083500000000     c                   MOVE      a5            a1                             *ultimo carattere
083600000000if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
083700000000     c                   MOVEL     'S'           $OkNum                         *valido
083800000000e   5c                   ENDIF
083900000000e   4c                   ENDIF
084000000000if  4c                   IF        $OkNum = 'N'                                 *NON valido
084100000000     c                   MOVEL     '1'           werr                           *errore Output
084200000000     c                   LEAVE
084300000000e   4c                   ENDIF
084400000000     c*
084500000000     c*
084600000000     c* sposta il campo in quello "normalizzato"
084700000000     c                   MOVEL     a5            NInpo107Vlm                    *campo"normalizzato"
084800000000e   3c                   ENDDO
084900000000e   2c                   ENDIF
085000000000     c*
085100000000     c* se errore, memorizza il codice errore nella schiera errori
085200000000if  2c                   IF        werr = '1'
085300000000if  3c                   IF        $e < 10                                      *non + di 10 errori
085400000000     c                   ADD       1             $e
085500000000     c                   MOVEL     '7'           Serr($e)                       *aggiunge errore
085600060605     c*                  MOVEL     msg(7)        Smsg($e)                       *messaggio
085700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0610'
085800060609     C                             :inpO107Lang)
085900000000     c                   MOVEL     '1'           $err                           *controllore errore
086000000000e   3c                   ENDIF
086100000000e   2c                   ENDIF
086200000000e   1c                   ENDIF
086300000000     c*---
086400000000     c* Numero bancali
086500000000     c*---
086600000000     c*
086700000000     c* controlla il campo solo se immesso
086800000000if  1c                   IF        werr = '0'
086900000000if  2c                   IF        Inpo107Bnc <> *blanks
087000000000do  3c                   DO
087100000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
087200000000     c                   SETOFF                                       98
087300000000     c                   TESTN                   Inpo107Bnc           98
087400000000if  4c                   IF        *in98                                        *da TESTN è numero
087500000000     c                   MOVE      Inpo107Bnc    a1                             *ultimo carattere
087600000000if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
087700000000     c                   MOVEL     'S'           $OkNum                         *valido
087800000000e   5c                   ENDIF
087900000000e   4c                   ENDIF
088000000000if  4c                   IF        $OkNum = 'N'                                 *NON valido
088100000000     c                   MOVEL     '1'           werr                           *errore Output
088200000000     c                   LEAVE
088300000000e   4c                   ENDIF
088400000000     c*
088500000000     c* sposta il campo in quello "normalizzato"
088600000000     c                   MOVEL     Inpo107Bnc    NInpo107Bnc                    *campo"normalizzato"
088700000000     c*
088800000000e   3c                   ENDDO
088900000000e   2c                   ENDIF
089000000000     c*
089100000000     c* se errore, memorizza il codice errore nella schiera errori
089200000000if  2c                   IF        werr = '1'
089300000000if  3c                   IF        $e < 10                                      *non + di 10 errori
089400000000     c                   ADD       1             $e
089500000000     c                   MOVEL     '8'           Serr($e)                       *aggiunge errore
089600060605     c*                  MOVEL     msg(8)        Smsg($e)                       *messaggio
089700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0408'
089800060609     C                             :inpO107Lang)
089900000000     c                   MOVEL     '1'           $err                           *controllore errore
090000000000e   3c                   ENDIF
090100000000e   2c                   ENDIF
090200000000e   1c                   ENDIF
090300000000     c*---
090400000000     c* Peso + Volume + Bancali
090500000000     c*---
090600000000     c*
090700000000     c* occorre immettere almeno un valore fra questi
090800000000if  1c                   IF        werr = '0'
090900000000do  2c                   DO
091000000000if  3c                   IF        Inpo107Pkg > *zeros OR
091100000000     c                             Inpo107Vlm > *zeros  OR
091200000000     c                             Inpo107Bnc > *zeros
091300000000x   3c                   ELSE
091400000000     c                   MOVEL     '1'           werr                           *errore Output
091500000000     c                   LEAVE
091600000000e   3c                   ENDIF
091700000000     c*
091800000000e   2c                   ENDDO
091900000000     c*
092000000000     c* se errore, memorizza il codice errore nella schiera errori
092100000000if  2c                   IF        werr = '1'
092200000000if  3c                   IF        $e < 10                                      *non + di 10 errori
092300000000     c                   ADD       1             $e
092400000000     c                   MOVEL     '9'           Serr($e)                       *aggiunge errore
092500060605     c*                  MOVEL     msg(9)        Smsg($e)                       *messaggio
092600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0235'
092700060609     C                             :inpO107Lang)
092800000000     c                   MOVEL     '1'           $err                           *controllore errore
092900000000e   3c                   ENDIF
093000000000e   2c                   ENDIF
093100000000e   1c                   ENDIF
093200000000     c*---
093300000000     c* Colli
093400000000     c*---
093500000000     c*
093600000000     c* controlla il campo solo se immesso
093700000000if  1c                   IF        werr = '0'
093800000000if  2c                   IF        Inpo107Ncl <> *blanks
093900000000do  3c                   DO
094000000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
094100000000     c                   SETOFF                                       98
094200000000     c                   TESTN                   Inpo107Ncl           98
094300000000if  4c                   IF        *in98                                        *da TESTN è numero
094400000000     c                   MOVE      Inpo107Ncl    a1                             *ultimo carattere
094500000000if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
094600000000     c                   MOVEL     'S'           $OkNum                         *valido
094700000000e   5c                   ENDIF
094800000000e   4c                   ENDIF
094900000000if  4c                   IF        $OkNum = 'N'                                 *NON valido
095000000000     c                   MOVEL     '1'           werr                           *errore Output
095100000000     c                   LEAVE
095200000000e   4c                   ENDIF
095300000000     c*
095400000000     c* sposta il campo in quello "normalizzato"
095500000000     c                   MOVEL     Inpo107Ncl    NInpo107Ncl                    *campo"normalizzato"
095600000000     c*
095700000000e   3c                   ENDDO
095800000000e   2c                   ENDIF
095900000000     c*
096000000000     c* se errore, memorizza il codice errore nella schiera errori
096100000000if  2c                   IF        werr = '1'
096200000000if  3c                   IF        $e < 10                                      *non + di 10 errori
096300000000     c                   ADD       1             $e
096400000000     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
096500060605     c*                  MOVEL     msg(10)       Smsg($e)                       *messaggio
096600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0410'
096700060609     C                             :inpO107Lang)
096800000000     c                   MOVEL     '1'           $err                           *controllore errore
096900000000e   3c                   ENDIF
097000000000e   2c                   ENDIF
097100000000e   1c                   ENDIF
097200090210     ** Se richiesto il ritiro con DPD
097300090210     ** i colli diventano obbligatori e devono essere 1.
097400090210     C                   IF        wErr = '0' AND
097500090210     C                             inpo107sse = 'D' AND
097600090210     C                             NInpo107Ncl <> 1
097700090210     c                   IF        $e < 10                                      *non + di 10 errori
097800090210     c                   ADD       1             $e
097900090210     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
098000090211     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0693'
098100090211     C                             : inpO107Lang)
098200090210     c                   MOVEL     '1'           $err                           *controllore errore
098300090210     c                   ENDIF
098400090210     C                   ENDIF
098500000000     c*---
098600000000     c* Bilici
098700000000     c*---
098800000000     c*
098900000000     c* controlla il campo solo se immesso
099000000000if  1c                   IF        werr = '0'
099100000000if  2c                   IF        Inpo107Blc <> *blanks
099200000000do  3c                   DO
099300000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
099400000000     c                   SETOFF                                       98
099500000000     c                   TESTN                   Inpo107Blc           98
099600000000if  4c                   IF        *in98                                        *da TESTN è numero
099700000000     c                   MOVE      Inpo107Blc    a1                             *ultimo carattere
099800000000if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
099900000000     c                   MOVEL     'S'           $OkNum                         *valido
100000000000e   5c                   ENDIF
100100000000e   4c                   ENDIF
100200000000if  4c                   IF        $OkNum = 'N'                                 *NON valido
100300000000     c                   MOVEL     '1'           werr                           *errore Output
100400000000     c                   LEAVE
100500000000e   4c                   ENDIF
100600000000     c*
100700000000     c* sposta il campo in quello "normalizzato"
100800000000     c                   MOVEL     Inpo107Blc    NInpo107Blc                    *campo"normalizzato"
100900000000     c*
101000000000e   3c                   ENDDO
101100000000e   2c                   ENDIF
101200000000     c*
101300000000     c* se errore, memorizza il codice errore nella schiera errori
101400000000if  2c                   IF        werr = '1'
101500000000if  3c                   IF        $e < 10                                      *non + di 10 errori
101600000000     c                   ADD       1             $e
101700000000     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
101800060605     c*                  MOVEL     msg(11)       Smsg($e)                       *messaggio
101900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0409'
102000060609     C                             :inpO107Lang)
102100000000     c                   MOVEL     '1'           $err                           *controllore errore
102200000000e   3c                   ENDIF
102300000000e   2c                   ENDIF
102400000000e   1c                   ENDIF
102500000000     c*---
102600000000     c* Autotreni
102700000000     c*---
102800000000     c*
102900000000     c* controlla il campo solo se immesso
103000000000if  1c                   IF        werr = '0'
103100000000if  2c                   IF        Inpo107Att <> *blanks
103200000000do  3c                   DO
103300000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
103400000000     c                   SETOFF                                       98
103500000000     c                   TESTN                   Inpo107Att           98
103600000000if  4c                   IF        *in98                                        *da TESTN è numero
103700000000     c                   MOVE      Inpo107Att    a1                             *ultimo carattere
103800000000if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
103900000000     c                   MOVEL     'S'           $OkNum                         *valido
104000000000e   5c                   ENDIF
104100000000e   4c                   ENDIF
104200000000if  4c                   IF        $OkNum = 'N'                                 *NON valido
104300000000     c                   MOVEL     '1'           werr                           *errore Output
104400000000     c                   LEAVE
104500000000e   4c                   ENDIF
104600000000     c*
104700000000     c* sposta il campo in quello "normalizzato"
104800000000     c                   MOVEL     Inpo107Att    NInpo107Att                    *campo"normalizzato"
104900000000     c*
105000000000e   3c                   ENDDO
105100000000e   2c                   ENDIF
105200000000     c*
105300000000     c* se errore, memorizza il codice errore nella schiera errori
105400000000if  2c                   IF        werr = '1'
105500000000if  3c                   IF        $e < 10                                      *non + di 10 errori
105600000000     c                   ADD       1             $e
105700000000     c                   MOVEL     'C'           Serr($e)                       *aggiunge errore
105800060605     c*                  MOVEL     msg(12)       Smsg($e)                       *messaggio
105900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0407'
106000060609     C                             :inpO107Lang)
106100000000     c                   MOVEL     '1'           $err                           *controllore errore
106200000000e   3c                   ENDIF
106300000000e   2c                   ENDIF
106400000000e   1c                   ENDIF
106500000000     c*---
106600000000     c* Motrici
106700000000     c*---
106800000000     c*
106900000000     c* controlla il campo solo se immesso
107000000000if  1c                   IF        werr = '0'
107100000000if  2c                   IF        Inpo107Mtc <> *blanks
107200000000do  3c                   DO
107300000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
107400000000     c                   SETOFF                                       98
107500000000     c                   TESTN                   Inpo107Mtc           98
107600000000if  4c                   IF        *in98                                        *da TESTN è numero
107700000000     c                   MOVE      Inpo107Mtc    a1                             *ultimo carattere
107800000000if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
107900000000     c                   MOVEL     'S'           $OkNum                         *valido
108000000000e   5c                   ENDIF
108100000000e   4c                   ENDIF
108200000000if  4c                   IF        $OkNum = 'N'                                 *NON valido
108300000000     c                   MOVEL     '1'           werr                           *errore Output
108400000000     c                   LEAVE
108500000000e   4c                   ENDIF
108600000000     c*
108700000000     c* sposta il campo in quello "normalizzato"
108800000000     c                   MOVEL     Inpo107Mtc    NInpo107Mtc                    *campo"normalizzato"
108900000000     c*
109000000000e   3c                   ENDDO
109100000000e   2c                   ENDIF
109200000000     c*
109300000000     c* se errore, memorizza il codice errore nella schiera errori
109400000000if  2c                   IF        werr = '1'
109500000000if  3c                   IF        $e < 10                                      *non + di 10 errori
109600000000     c                   ADD       1             $e
109700000000     c                   MOVEL     'D'           Serr($e)                       *aggiunge errore
109800060605     c*                  MOVEL     msg(13)       Smsg($e)                       *messaggio
109900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0411'
110000060609     C                             :inpO107Lang)
110100000000     c                   MOVEL     '1'           $err                           *controllore errore
110200000000e   3c                   ENDIF
110300000000e   2c                   ENDIF
110400000000e   1c                   ENDIF
110500000000     c*
110600000000     c                   ENDSR
110700000000     c*--------------------------------------------------------------------------------------------*
110800000000     c* controlla la quanità merce
110900000000     c*--------------------------------------------------------------------------------------------*
111000000000     c     ChkInpQlt     BEGSR
111100000000     c*
111200000000     c* reimposta le variabili di lavoro
111300000000     c                   MOVEL     '0'           werr                           *NO errore
111400000000     c                   CLEAR                   NInpo107Nam                    *campi"normalizzati"
111500000000     c*
111600000000     c* campo obbligatorio se non immesso il cliente ritiro (clienti CON e SENZA password)
111700000000do  1c                   DO
111800000000if  2c                   IF        Inpo107Cro > *zeros                          *cl.ritiro IMMESSO
111900000000x   2c                   ELSE                                                   *cl.ritiro NON IMM.
112000000000if  3c                   IF        Inpo107Nam = *blanks
112100000000     c                   MOVEL     '1'           werr                           *errore Output
112200000000     c                   LEAVE
112300000000e   3c                   ENDIF
112400000000e   2c                   ENDIF
112500000000     c*
112600000000     c* sposta il campo in quello "normalizzato"
112700000000     c     minu:maiu     XLATE     Inpo107Nam    NInpo107Nam                    *Minus -> Maiuscolo
112800000000     c*
112900000000e   1c                   ENDDO
113000000000     c*
113100000000     c* se errore, memorizza il codice errore nella schiera errori
113200000000if  1c                   IF        werr = '1'
113300000000if  2c                   IF        $e < 10                                      *non + di 10 errori
113400000000     c                   ADD       1             $e
113500000000     c                   MOVEL     'S'           Serr($e)                       *aggiunge errore
113600060605     c*                  MOVEL     msg(26)       Smsg($e)                       *messaggio
113700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0143'
113800060609     C                             :inpO107Lang)
113900000000     c                   MOVEL     '1'           $err                           *controllore errore
114000000000e   2c                   ENDIF
114100000000e   1c                   ENDIF
114200000000     c*
114300000000     c                   ENDSR
114400000000     c*--------------------------------------------------------------------------------------------*
114500000000     c* controlla la persona da contattare
114600000000     c*--------------------------------------------------------------------------------------------*
114700000000     c     ChkInpRer     BEGSR
114800000000     c*
114900000000     c* reimposta le variabili di lavoro
115000000000     c                   MOVEL     '0'           werr                           *NO errore
115100000000     c                   CLEAR                   NInpo107Rer                    *campi"normalizzati"
115200000000     c                   CLEAR                   NInpo107TeR
115300000000     c*
115400000000     c* campo obbligatorio se non immesso il cliente ritiro (clienti CON e SENZA password)
115500000000do  1c                   DO
115600050316if  2c*                  IF        Inpo107Cro > *zeros                          *cl.ritiro IMMESSO
115700050316x   2c*                  ELSE                                                   *cl.ritiro NON IMM.
115800050316if  3c*                  IF        Inpo107Rer <> *blanks AND
115900050316     c*                            Inpo107Ter <> *blanks
116000050316x   3c*                  ELSE
116100050316     c*                  MOVEL     '1'           werr                           *errore Output
116200050316     c*                  LEAVE
116300050316e   3c*                  ENDIF
116400050316e   2c*                  ENDIF
116500050316     ** Referente per ritiro obbligatorio se non immesso codice ritiro
116600050316     ** oppure se richiesto il contatto telefonico preventivo.
116700050316if  3C                   IF        (InpO107Rer = *BLANK OR InpO107Ter = *BLANK)
116800050316if  2C                             AND
116900050316if  2C                             (InpO107Cro = *ZERO OR InpO107CttTlf = 'S')
117000050316     C                   EVAL      WErr = *ON
117100050316     C                   LEAVE
117200050316e   3C                   ENDIF
117300000000     c*
117400000000     c* sposta il campo in quello "normalizzato"
117500000000     c     minu:maiu     XLATE     Inpo107Rer    NInpo107Rer                    *Minus -> Maiuscolo
117600000000     c     minu:maiu     XLATE     Inpo107Ter    NInpo107Ter                    *Minus -> Maiuscolo
117700000000e   1c                   ENDDO
117800000000     c*
117900000000     c* se errore, memorizza il codice errore nella schiera errori
118000000000if  1c                   IF        werr = '1'
118100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
118200000000     c                   ADD       1             $e
118300000000     c                   MOVEL     'E'           Serr($e)                       *aggiunge errore
118400060605     c*                  MOVEL     msg(14)       Smsg($e)                       *messaggio
118500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0290'
118600060609     C                             :inpO107Lang)
118700000000     c                   MOVEL     '1'           $err                           *controllore errore
118800000000e   2c                   ENDIF
118900000000e   1c                   ENDIF
119000000000     c*
119100000000     c                   ENDSR
119200000000     c*--------------------------------------------------------------------------------------------*
119300000000     c* controlla consegna a uno o più destinatari
119400000000     c*--------------------------------------------------------------------------------------------*
119500000000     c     ChkInpUpd     BEGSR
119600000000     c*
119700000000     c* reimposta le variabili di lavoro
119800000000     c                   MOVEL     '0'           werr                           *NO errore
119900011108     c                   CLEAR                   wmsg                           *msg impostato
120000011108     c*
120100011108do  1c                   DO
120200000000     c*
120300000000     c* controlla validità campo
120400000000if  2c                   IF        Inpo107Upd = '1' OR
120500000000     c                             Inpo107Upd = '2'
120600000000x   2c                   ELSE
120700011108     c                   MOVEL     '1'           werr                           *errore Output
120800060605     c*                  MOVEL     msg(15)       wmsg                           *messaggio
120900060609     C                   EVAL      wmsg = rtvMsgLang('TIS0295'
121000060609     C                             :inpO107Lang)
121100000000     c                   LEAVE
121200000000e   2c                   ENDIF
121300011108     c*
121400011108     c* se prepagato, DEVE essere UN destinatario
121500011108if  2c                   IF        InpO107Ppg = 'S' AND
121600011108     c                             Inpo107Upd <> '1'
121700011108     c                   MOVEL     '1'           werr                           *errore Output
121800060605     c*                  MOVEL     msg(28)       wmsg                           *messaggio
121900060609     C                   EVAL      wmsg = rtvMsgLang('TIS0307'
122000060609     C                             :inpO107Lang)
122100011108     c                   LEAVE
122200011108e   2c                   ENDIF
122300101201     c*
122400101201     c* Se DPD, DEVE essere UN destinatario
122500101201if  2c                   IF        InpO107sse = 'D' AND
122600101201     c                             Inpo107Upd <> '1'
122700101201     c                   MOVEL     '1'           werr                           *errore Output
122800101201     C                   EVAL      wmsg = rtvMsgLang('TIS0307'
122900101201     C                             :inpO107Lang)
123000101201     c                   LEAVE
123100101201e   2c                   ENDIF
123200011108     c*
123300000000e   1c                   ENDDO
123400011108     c*
123500011108     c* se errore, memorizza il codice errore nella schiera errori
123600011108if  1c                   IF        werr = '1'
123700011108if  2c                   IF        $e < 10                                      *non + di 10 errori
123800011108     c                   ADD       1             $e
123900011108     c                   MOVEL     'F'           Serr($e)                       *aggiunge errore
124000011108     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
124100011108     c                   MOVEL     '1'           $err                           *controllore errore
124200011108e   2c                   ENDIF
124300011108e   1c                   ENDIF
124400000000     c*
124500000000     c                   ENDSR
124600000000     c*--------------------------------------------------------------------------------------------*
124700000000     c* controlla il destinatario
124800000000     c*--------------------------------------------------------------------------------------------*
124900000000     c     ChkInpDes     BEGSR
125000000000     c*
125100000000     c* reimposta le variabili di lavoro
125200000000     c                   MOVEL     '0'           werr                           *NO errore
125300000000do  1c                   DO
125400021029     c*
125500021029     c* se la consegna è singola  il destinatario è da inserire
125600000000if  2c                   IF        Inpo107Upd = '1'                             *singola
125700000000if  3c                   IF        Inpo107Rsc = *blanks OR
125800000000     c                             Inpo107Inc = *blanks OR
125900000000     c                             Inpo107Loc = *blanks OR
126000000000     c                             Inpo107Cac = *blanks
126100000000     c                   MOVEL     '1'           werr                           *errore Output
126200060605     c*                  MOVEL     msg(16)       wmsg                           *messaggio
126300060609     C                   EVAL      wmsg = rtvMsgLang('TIS0288'
126400060609     C                             :inpO107Lang)
126500060609e   3c                   ENDIF
126600000000e   2c                   ENDIF
126700021029     c*
126800021029     c* se la consegna è multipla il destinatario è da NON inserire
126900021029if  2c                   IF        Inpo107Upd = '2'                             *multipla
127000021029if  3c                   IF        Inpo107Rsc <> *blanks OR
127100021029     c                             Inpo107Inc <> *blanks OR
127200021029     c                             Inpo107Loc <> *blanks OR
127300021029     c                             Inpo107Cac <> *blanks
127400021029     c                   MOVEL     '1'           werr                           *errore Output
127500060605     c*                  MOVEL     msg(30)       wmsg                           *messaggio
127600060609     C                   EVAL      wmsg = rtvMsgLang('TIS0380'
127700060609     C                             :inpO107Lang)
127800060609e   3c                   ENDIF
127900021029e   2c                   ENDIF
128000000000     C*
128100000000e   1C                   ENDDO
128200000000     c*
128300000000     c* se errore, memorizza il codice errore nella schiera errori
128400000000if  1c                   IF        werr = '1'
128500000000if  2c                   IF        $e < 10                                      *non + di 10 errori
128600000000     c                   ADD       1             $e
128700000000     c                   MOVEL     'G'           Serr($e)                       *aggiunge errore
128800021029     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
128900000000     c                   MOVEL     '1'           $err                           *controllore errore
129000000000e   2c                   ENDIF
129100000000e   1c                   ENDIF
129200000000     c*---
129300000000     c* Nominativo
129400000000     c*---
129500000000if  1c                   IF        Inpo107Upd = '1'                             *singola
129600000000if  2c                   IF        werr <> '1'                                  *NO errori
129700000000     c                   EXSR      ChkInpRsc
129800000000e   2c                   ENDIF
129900000000     c*---
130000000000     c* Indirizzo
130100000000     c*---
130200000000if  2c                   IF        werr <> '1'                                  *NO errori
130300000000     c                   EXSR      ChkInpInc
130400000000e   2c                   ENDIF
130500000000     c*---
130600000000     c* Nazione
130700000000     c*---
130800000000if  2c                   IF        werr <> '1'                                  *NO errori
130900000000     c                   EXSR      ChkInpNac
131000000000e   2c                   ENDIF
131100000000     c*---
131200000000     c* CAP
131300000000     c*---
131400000000if  2c                   IF        werr <> '1'                                  *NO errori
131500000000     c                   EXSR      ChkInpCac
131600000000e   2c                   ENDIF
131700000000     c*---
131800000000     c* Località
131900000000     c*---
132000000000if  2c                   IF        werr <> '1'                                  *NO errori
132100000000     c                   EXSR      ChkInpLoc
132200000000e   2c                   ENDIF
132300000000     c*---
132400000000     c* provincia
132500000000     c*---
132600000000if  2c                   IF        werr <> '1'                                  *NO errori
132700000000     c                   EXSR      ChkInpPrc
132800000000e   2c                   ENDIF
132900000000     c*---
133000000000     c* Indirizzo + il CAP + la località + la provincia + la nazione
133100000000     c*---
133200000000if  2c                   IF        werr <> '1'                                  *NO errori
133300000000     c                   EXSR      ChkInpICKPNc
133400000000e   2c                   ENDIF
133500000000e   1c                   ENDIF
133600000000     c*
133700000000     c                   ENDSR
133800000000     c*--------------------------------------------------------------------------------------------*
133900000000     c* controlla il nominativo destinatario
134000000000     c*--------------------------------------------------------------------------------------------*
134100000000     c     ChkInpRsc     BEGSR
134200000000     c*
134300000000     c* reimposta le variabili di lavoro
134400000000     c                   MOVEL     '0'           werr                           *NO errore
134500000000     c                   CLEAR                   NInpo107Rsc                    *campo"normalizzato"
134600000000     c*
134700000000     c* controlla l'obbligatorietà del campo
134800000000do  1c                   DO
134900000000if  2c                   IF        Inpo107Rsc = *blanks
135000000000     c                   MOVEL     '1'           werr                           *SI errore
135100000000     c                   LEAVE
135200000000e   2c                   ENDIF
135300000000     C*
135400000000e   1c                   ENDDO
135500000000     c*
135600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
135700000000     c     minu:maiu     XLATE     Inpo107Rsc    NInpo107Rsc                    *Minus -> Maiuscolo
135800000000     c*
135900000000     c* se errore, memorizza il codice errore nella schiera errori
136000000000if  1c                   IF        werr = '1'
136100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
136200000000     c                   ADD       1             $e
136300000000     c                   MOVEL     'H'           Serr($e)                       *aggiunge errore
136400060605     c*                  MOVEL     msg(17)       Smsg($e)                       *messaggio
136500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0481'
136600060609     C                             :inpO107Lang)
136700000000     c                   MOVEL     '1'           $err                           *controllore errore
136800000000e   2c                   ENDIF
136900000000e   1c                   ENDIF
137000000000     c*
137100000000     c                   ENDSR
137200000000     c*--------------------------------------------------------------------------------------------*
137300000000     c* controlla l'indirizzo destinatario
137400000000     c*--------------------------------------------------------------------------------------------*
137500000000     c     ChkInpInc     BEGSR
137600000000     c*
137700000000     c* reimposta le variabili di lavoro
137800000000     c                   MOVEL     '0'           werr                           *NO errore
137900000000     c                   CLEAR                   NInpo107Inc                    *campo"normalizzato"
138000000000     c*
138100000000     c* controlla l'obbligatorietà del campo
138200000000do  1c                   DO
138300000000if  2c                   IF        Inpo107Inc = *blanks
138400000000     c                   MOVEL     '1'           werr                           *SI errore
138500000000     c                   LEAVE
138600000000e   2c                   ENDIF
138700000000     c*
138800000000e   1C                   ENDDO
138900000000     c*
139000000000     c* trasforma il campo immesso da minuscolo a maiuscolo
139100000000     c     minu:maiu     XLATE     Inpo107Inc    NInpo107Inc                    *Minus -> Maiuscolo
139200000000     c*
139300000000     c* se errore, memorizza il codice errore nella schiera errori
139400000000if  1c                   IF        werr = '1'
139500000000if  2c                   IF        $e < 10                                      *non + di 10 errori
139600000000     c                   ADD       1             $e
139700000000     c                   MOVEL     'I'           Serr($e)                       *aggiunge errore
139800060605     c*                  MOVEL     msg(18)       Smsg($e)                       *messaggio
139900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0282'
140000060609     C                             :inpO107Lang)
140100000000     c                   MOVEL     '1'           $err                           *controllore errore
140200000000e   2c                   ENDIF
140300000000e   1c                   ENDIF
140400000000     c*
140500000000     c                   ENDSR
140600000000     c*--------------------------------------------------------------------------------------------*
140700000000     c* controlla la nazione destinatario
140800000000     c*--------------------------------------------------------------------------------------------*
140900000000     c     ChkInpNac     BEGSR
141000000000     c*
141100000000     c* reimposta le variabili di lavoro
141200000000     c                   MOVEL     '0'           werr                           *NO errore
141300000000     c                   CLEAR                   NInpo107Nac                    *campo"normalizzato"
141400000000     c                   CLEAR                   deplad                         *deposito
141500020517     c                   CLEAR                   wmsg                           *msg impostato
141600000000     c*
141700000000     c* trasforma il campo immesso da minuscolo a maiuscolo
141800000000do  1c                   DO
141900000000     c     minu:maiu     XLATE     Inpo107Nac    NInpo107Nac                    *Minus -> Maiuscolo
142000000000     c*
142100000000     c* controlla esistenza codice in anagrafica
142200000000if  2c                   IF        NInpo107Nac <> *blanks                       *se inserito
142300000000     c                   SETOFF                                           98
142400000000     c                   Z-ADD     1             i
142500000000     c     NInpo107Nac   LOOKUP    SNaz(i)                                98
142600000000if  3c                   IF        NOT *in98                                    *NON trovata
142700000000     c                   MOVEL     '1'           werr                           *SI errore
142800060605     c*                  MOVEL     msg(22)       wmsg                           *messaggio
142900060609     C                   EVAL      wmsg = rtvMsgLang('TIS0385'
143000060609     C                             :inpO107Lang)
143100060609     c                   LEAVE
143200000000x   3c                   ELSE
143300000000     C                   Z-ADD     SNLad(i)      deplad                         *l.arrivo DPD
143400000000e   3c                   ENDIF
143500000000e   2c                   ENDIF
143600020517     c*
143700020517     c* se cliente NON codificato + consegna all'estero + pagamento destinatario, errore
143800020517if  2c                   IF        NInpo107Nac <> *blanks AND                   *se inserito
143900020517     c                             InpO107Ksu  <= *zeros  AND                   *NO cliente PW
144000020517     c                             InpO107Pag = 'D'                             *pagam.destinatario
144100020517     c                   MOVEL     '1'           werr                           *errore Output
144200060605     c*                  MOVEL     msg(29)       wmsg                           *messaggio
144300060609     C                   EVAL      wmsg = rtvMsgLang('TIS0448'
144400060609     C                             :inpO107Lang)
144500060609e   2c                   ENDIF
144600000000     C*
144700000000e   1c                   ENDDO
144800000000     c*
144900000000     c* se errore, memorizza il codice errore nella schiera errori
145000000000if  1c                   IF        werr = '1'
145100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
145200000000     c                   ADD       1             $e
145300000000     c                   MOVEL     'O'           Serr($e)                       *aggiunge errore
145400020517     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
145500000000     c                   MOVEL     '1'           $err                           *controllore errore
145600000000e   2c                   ENDIF
145700000000e   1c                   ENDIF
145800000000     c*
145900000000     c                   ENDSR
146000000000     c*--------------------------------------------------------------------------------------------*
146100000000     c* controlla il CAP destinatario
146200000000     c*--------------------------------------------------------------------------------------------*
146300000000     c     ChkInpCac     BEGSR
146400000000     c*
146500000000     c* reimposta le variabili di lavoro
146600000000     c                   MOVEL     '0'           werr                           *NO errore
146700000000     c                   CLEAR                   NInpo107cac                    *campo"normalizzato"
146800000000     c*
146900000000     c* controlla l'obbligatorietà del campo
147000000000do  1c                   DO
147100000000if  2c                   IF        Inpo107cac = *blanks
147200000000     c                   MOVEL     '1'           werr                           *SI errore
147300000000     c                   LEAVE
147400000000e   2c                   ENDIF
147500000000     c*
147600000000e   1C                   ENDDO
147700000000     C*
147800000000     c* trasforma il campo immesso da minuscolo a maiuscolo
147900000000     c     minu:maiu     XLATE     Inpo107Cac    NInpo107Cac                    *Minus -> Maiuscolo
148000000000     c*
148100000000     c* se errore, memorizza il codice errore nella schiera errori
148200000000if  1c                   IF        werr = '1'
148300000000if  2c                   IF        $e < 10                                      *non + di 10 errori
148400000000     c                   ADD       1             $e
148500000000     c                   MOVEL     'L'           Serr($e)                       *aggiunge errore
148600060605     c*                  MOVEL     msg(19)       Smsg($e)                       *messaggio
148700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0052'
148800060609     C                             :inpO107Lang)
148900060609     c                   MOVEL     '1'           $err                           *controllore errore
149000000000e   2c                   ENDIF
149100000000e   1c                   ENDIF
149200000000     c*
149300000000     c                   ENDSR
149400000000     c*--------------------------------------------------------------------------------------------*
149500000000     c* controlla la località destinatario
149600000000     c*--------------------------------------------------------------------------------------------*
149700000000     c     ChkInpLoc     BEGSR
149800000000     c*
149900000000     c* reimposta le variabili di lavoro
150000000000     c                   MOVEL     '0'           werr                           *NO errore
150100000000     c                   CLEAR                   NInpo107Loc                    *campo"normalizzato"
150200000000     c*
150300000000     c* controlla l'obbligatorietà del campo
150400000000do  1c                   DO
150500000000if  2c                   IF        Inpo107loc = *blanks
150600000000     c                   MOVEL     '1'           werr                           *SI errore
150700000000     c                   LEAVE
150800000000e   2c                   ENDIF
150900000000     c*
151000000000e   1C                   ENDDO
151100000000     C*
151200000000     c* trasforma il campo immesso da minuscolo a maiuscolo
151300000000     c     minu:maiu     XLATE     Inpo107Loc    NInpo107Loc                    *Minus -> Maiuscolo
151400000000     c*
151500000000     c* se errore, memorizza il codice errore nella schiera errori
151600000000if  1c                   IF        werr = '1'
151700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
151800000000     c                   ADD       1             $e
151900000000     c                   MOVEL     'M'           Serr($e)                       *aggiunge errore
152000060605     c*                  MOVEL     msg(20)       Smsg($e)                       *messaggio
152100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0346'
152200060609     C                             :inpO107Lang)
152300000000     c                   MOVEL     '1'           $err                           *controllore errore
152400000000e   2c                   ENDIF
152500000000e   1c                   ENDIF
152600000000     c*
152700000000     c                   ENDSR
152800000000     c*--------------------------------------------------------------------------------------------*
152900000000     c* controlla la provincia destinatario
153000000000     c*--------------------------------------------------------------------------------------------*
153100000000     c     ChkInpPrc     BEGSR
153200000000     c*
153300000000     c* reimposta le variabili di lavoro
153400000000     c                   MOVEL     '0'           werr                           *NO errore
153500000000     c                   CLEAR                   NInpo107Prc                    *campo"normalizzato"
153600000000     c*
153700000000     c* controlla l'obbligatorietà del campo
153800000000do  1c                   DO
153900000000     c*
154000000000     c* trasforma il campo immesso da minuscolo a maiuscolo
154100000000     c     minu:maiu     XLATE     Inpo107Prc    NInpo107Prc                    *Minus -> Maiuscolo
154200000000     c*
154300000000     c* controlla esistenza codice in anagrafica
154400000000if  2c                   IF        NInpo107Prc <> *blanks                       *se inserito
154500000000     c                   SETOFF                                           98
154600000000     c                   Z-ADD     1             i
154700000000     c     NInpo107Prc   LOOKUP    SPrv(i)                                98
154800000000if  3c                   IF        NOT *in98                                    *NON trovata
154900000000     c                   MOVEL     '1'           werr                           *SI errore
155000000000     c                   LEAVE
155100000000e   3c                   ENDIF
155200000000e   2c                   ENDIF
155300000000     c*
155400000000e   1C                   ENDDO
155500000000     c*
155600000000     c* se errore, memorizza il codice errore nella schiera errori
155700000000if  1c                   IF        werr = '1'
155800000000if  2c                   IF        $e < 10                                      *non + di 10 errori
155900000000     c                   ADD       1             $e
156000000000     c                   MOVEL     'N'           Serr($e)                       *aggiunge errore
156100060605     c*                  MOVEL     msg(21)       Smsg($e)                       *messaggio
156200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0464'
156300060609     C                             :inpO107Lang)
156400060609     c                   MOVEL     '1'           $err                           *controllore errore
156500000000e   2c                   ENDIF
156600000000e   1c                   ENDIF
156700000000     c*
156800000000     c                   ENDSR
156900000000     c*--------------------------------------------------------------------------------------------*
157000000000     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione destinatario
157100000000     c*--------------------------------------------------------------------------------------------*
157200000000     c     ChkInpICKPNc  BEGSR
157300000000     c*
157400000000     c* reimposta le variabili di lavoro
157500000000     c                   MOVEL     '0'           werr                           *NO errore
157600000000     c                   Z-ADD     *zeros        deplna                         *deposito LNA
157700000000     c*
157800000000     c* controlla esattezza dati destinatario
157900000000do  1c                   DO
158000000000     C                   CLEAR                   tisi95ds
158100000000     C                   MOVEL     ' '           I95tla
158200000000     C                   MOVEL     '7'           I95tcn
158300020517     C                   MOVEL     NInpo107nac   I95nar
158400000000     C                   MOVEL     NInpo107Cac   I95cap
158500000000     C                   MOVEL     NInpo107Loc   I95loc
158600000000     C                   MOVEL     NInpo107Prc   I95prv
158700000000     C                   MOVEL     NInpo107Inc   I95ind
158800000000     C                   Z-ADD     datcor        I95dat
158900120829     C                   IF        i95nar = IRLANDA
159000120829     C                   EVAL      i95cap = EIRE
159100120829     C                   ENDIF
159200120829     C                   CALL      'TISI95R'
159300000000     C                   PARM                    tisi95ds
159400000000     c*
159500000000     c* se ritorna errore o dato non affidabile imposta errore
159600000000if  2c                   IF        O95Err <> *blanks
159700000000     c                   MOVEL     '1'           werr                           *SI errore
159800000000     c                   LEAVE
159900000000x   2c                   ELSE                                                   *NO errore
160000000000if  3c                   IF        NInpo107Nac = *blanks  AND                   *ITALIA
160100000000     c                             O95Lia <> '3'          OR                    *affidabilità 3
160200000000     c                             NInpo107Nac <> *blanks AND                   *ESTERO
160300000000     c                             O95Lia <> '2'                                *affidabilità 2
160400000000     c                   MOVEL     '1'           werr                           *SI errore
160500000000     c                   LEAVE
160600000000e   3c                   ENDIF
160700000000e   2c                   ENDIF
160800000000     c*
160900000000     c                   Z-ADD     O95Lna        depLna                         *l.arrivo consegna
161000110713     ** Se lORM Export è per DPD faccio un ulteriore controllo, non do la
161100110713     ** possibilità di inserire il ritiro se il destinatario è di San Marino,
161200110713     ** Città del Vaticano o Livigno.
161300110713     C                   IF        inpo107sse = 'D'
161400110713     C                   RESET                   fior95ds
161500110713     C                   EVAL      ior95cap = NInpo107Cac
161600110713     C                   EVAL      ior95loc = NInpo107Loc
161700110713     C                   EVAL      ior95prv = NInpo107Prc
161800110713     C                   EVAL      ior95naz = NInpo107Nac
161900110713     C                   EVAL      ior95lin = cvtLinguaISO2ToTntbe(inpO107Lang)
162000110713     C                   MONITOR
162100110713     C                   CALL      'FIOR95R'
162200110713     C                   PARM                    kpjba
162300110713     C                   PARM                    fior95ds
162400110713     C                   ON-ERROR
162500110713     C                   DUMP(A)
162600110713     C                   ENDMON
162700110713     C                   IF        (oor95err <> *BLANK OR oor95ok = 'N')
162800110713     C                             AND $e < 10
162900110713     C                   EVAL      $e += 1
163000110713     C                   EVAL      sErr($e) = 'P'
163100110713     C                   EVAL      sMsg($e) = oor95msg
163200110713     C                   EVAL      $err = *ON
163300110713     C                   LEAVE
163400110713     C                   ENDIF
163500110713     C                   ENDIF
163600000000     c*
163700000000e   1c                   ENDDO
163800000000     c* se errore, memorizza il codice errore nella schiera errori
163900000000if  1c                   IF        werr = '1'
164000000000if  2c                   IF        $e < 10                                      *non + di 10 errori
164100000000     c                   ADD       1             $e
164200000000     c                   MOVEL     'P'           Serr($e)                       *aggiunge errore
164300060605     c*                  MOVEL     msg(23)       Smsg($e)                       *messaggio
164400060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0139'
164500060609     C                             :inpO107Lang)
164600060609     c                   MOVEL     '1'           $err                           *controllore errore
164700000000e   2c                   ENDIF
164800000000e   1c                   ENDIF
164900000000     C*
165000000000     C                   ENDSR
165100000000     c*--------------------------------------------------------------------------------------------*
165200000000     c* controlla consegna a uno o più destinatari
165300000000     c*--------------------------------------------------------------------------------------------*
165400000000     c     ChkInpFfd     BEGSR
165500000000     c*
165600000000     c* reimposta le variabili di lavoro
165700000000     c                   MOVEL     '0'           werr                           *NO errore
165800000000     c*
165900000000     c* controlla validità campo
166000000000do  1c                   DO
166100000000if  2c                   IF        Inpo107Ffd = 'S' OR
166200000000     c                             Inpo107Ffd = 'N'
166300000000x   2c                   ELSE
166400000000     c                   MOVEL     '1'           werr                           *errore Output
166500000000     c                   LEAVE
166600000000e   2c                   ENDIF
166700000000     c*
166800000000e   1c                   ENDDO
166900000000     c*
167000000000     c* se errore, memorizza il codice errore nella schiera errori
167100000000if  1c                   IF        werr = '1'
167200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
167300000000     c                   ADD       1             $e
167400000000     c                   MOVEL     'Q'           Serr($e)                       *aggiunge errore
167500060605     c*                  MOVEL     msg(24)       Smsg($e)                       *messaggio
167600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0537'
167700060609     C                             :inpO107Lang)
167800060609     c                   MOVEL     '1'           $err                           *controllore errore
167900000000e   2c                   ENDIF
168000000000e   1c                   ENDIF
168100000000     c*
168200000000     c                   ENDSR
168300000000     c*--------------------------------------------------------------------------------------------*
168400000000     c* controlla la scelta del network estero
168500000000     c*--------------------------------------------------------------------------------------------*
168600000000     c     ChkInpSse     BEGSR
168700000000     c*
168800000000     c* reimposta le variabili di lavoro
168900000000     c                   MOVEL     '0'           werr                           *NO errore
169000000000     c*
169100000000     c* controlla validità campo
169200000000do  1c                   DO
169300000000if  2c                   IF        Inpo107Sse = 'N' OR
169400000000     c                             Inpo107Sse = 'E' OR
169500081118     c                             Inpo107Sse = 'D' OR
169600081118     c                             Inpo107Sse = *BLANK
169700000000x   2c                   ELSE
169800000000     c                   MOVEL     '1'           werr                           *errore Output
169900000000     c                   LEAVE
170000000000e   2c                   ENDIF
170100000000     c*
170200000000e   1c                   ENDDO
170300000000     c*
170400000000     c* se errore, memorizza il codice errore nella schiera errori
170500000000if  1c                   IF        werr = '1'
170600000000if  2c                   IF        $e < 10                                      *non + di 10 errori
170700000000     c                   ADD       1             $e
170800000000     c                   MOVEL     'R'           Serr($e)                       *aggiunge errore
170900060605     c*                  MOVEL     msg(25)       Smsg($e)                       *messaggio
171000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0536'
171100060609     C                             :inpO107Lang)
171200060609     c                   MOVEL     '1'           $err                           *controllore errore
171300000000e   2c                   ENDIF
171400081118     c                   LEAVESR
171500000000e   1c                   ENDIF
171600081118     ** Verifica la ritirabilità all'estero.
171700081119     C                   IF        inpo107nar <> *BLANK AND
171800081119     C                             inpo107sse <> *BLANK AND inpo107sse <> 'N'
171900081118     C                   RESET                   fnlv12ds
172000081118     C                   RESET                   tisi95ds
172100081118     C                   RESET                   tisi97ds
172200081118     C                   EVAL      i12lang = inpo107lang
172300081118     C                   EVAL      i95nar = inpo107nar
172400081118     C                   EVAL      i95cap = inpo107car
172500091005     C                   MONITOR
172600081118     C                   EVAL      i95dat = %DEC(%DATE(inpo107dar : *EUR0))
172700091005     C                   ON-ERROR
172800091005     C                   EVAL      i95dat = %DEC(%DATE())
172900091005     C                   ENDMON
173000081118     C                   IF        inpo107sse = 'D'
173100081118     C                   EVAL      i97ntw = 'DPD'
173200081118     C                   ELSEIF    inpo107sse = 'E'
173300081118     C                   EVAL      i97ntw = 'EEX'
173400081118     C                   ENDIF
173500081118     C                   CALL      'FNLV12R'
173600081118     C                   PARM                    fnlv12ds
173700081118     C                   PARM                    tisi95ds
173800081118     C                   PARM                    tisi97ds
173900081118     C                   IF        o12err <> *BLANK AND $e < 10
174000081118     C                   EVAL      $e += 1
174100081118     C                   EVAL      sErr($e) = 'R'
174200081118     C                   EVAL      sMsg($e) = o12msg
174300081118     C                   EVAL      $err = '1'
174400081118     C                   ENDIF
174500081118     C                   ENDIF
174600081118     c*
174700000000     c                   ENDSR
174800000000     c*--------------------------------------------------------------------------------------------*
174900000000     c* edita data da aaaammgg in gg/mm/aaaa
175000000000     c*--------------------------------------------------------------------------------------------*
175100000000     c     edtdat        BEGSR
175200000000     c*
175300000000     c                   MOVEL     *blanks       wdate
175400000000     c*
175500000000     c                   MOVEL     wdat          dsdat
175600000000     c                   MOVEL     dsgio         dsgioe
175700000000     c                   MOVEL     '/'           dsvd1
175800000000     c                   MOVEL     dsmes         dsmese
175900000000     c                   MOVEL     '/'           dsvd2
176000000000     c                   MOVEL     dsann         dsanne
176100000000     c                   MOVEL     dsdate        wdate
176200000000     c*
176300000000     c                   ENDSR
176400000000     c*--------------------------------------------------------------------------------------------*
176500000000     c* edita ora da hhmm in hh:mm
176600000000     c*--------------------------------------------------------------------------------------------*
176700000000     c     edtora        BEGSR
176800000000     c*
176900000000     c                   MOVEL     *blanks       worae
177000000000     c*
177100000000     c                   MOVEL     wora          dsora
177200000000     c                   MOVEL     dshh          dshhe
177300000000     c                   MOVEL     ':'           dsvo1
177400000000     c                   MOVEL     dsmm          dsmme
177500000000     c                   MOVEL     dsorae        worae
177600000000     c*
177700000000     c                   ENDSR
177800000000     c*--------------------------------------------------------------------------------------------*
177900000000     c* imposta i campi della DS di Output
178000000000     c*--------------------------------------------------------------------------------------------*
178100000000     c     ImpDsOut      BEGSR
178200000000     c*******************
178300000000     c* Peso e Volume
178400000000     c*******************
178500000000     c                   EVAL      Outo107Pk2 = NInpo107Pkg
178600000000     c                   EVAL      Outo107Vl2 = NInpo107Vlm
178700000000     c*******************
178800000000     c* Note 1      -> Orari di apertura
178900000000     c*******************
179000000000if  1c                   IF        Inpo107AD1 <> *blanks OR
179100000000     c                             Inpo107AD2 <> *blanks
179200000000     c                   EVAL      OutO107No1 =  'Ap'
179300000000if  2c                   IF        Inpo107AD1 <> *blanks
179400000000     c                   EVAL      OutO107No1 =  %TRIM(OutO107No1) + ' ' +
179500000000     c                             NInpO107AD1 + ' ' + NInpO107AA1
179600000000e   2c                   ENDIF
179700000000if  2c                   IF        Inpo107AD2 <> *blanks
179800000000     c                   EVAL      OutO107No1 =  %TRIM(OutO107No1) + ' ' +
179900000000     c                             NInpO107AD2 + ' ' + NInpO107AA2
180000000000e   2c                   ENDIF
180100000000e   1c                   ENDIF
180200000000     c*******************
180300000000     c* PO consegna -> se immesso il destinatario --> PO calcolato da CAP consegna tenendo conto
180400000000     c*                che se scelto servizio DPD occorre forzare la linea DPD
180500000000     c*             -> se NON immesso il destinatario --> PO non si può calcolare ma se si è
180600000000     c*                scelto il network DPD occorre inserirlo nelle note
180700000000     c*******************
180800011017     c* Ora non lo calcola perchè non serve a nulla (poteva servire per calcolare la tassazione dei
180900011017     c* prepagati che per ora non c'è) e quindi viene sempre impostato a zero.
181000011017     c                   MOVEL     *zeros        OutO107Poc                     *NON calcolato
181100011017     c*
181200011017if  1c*---               IF        InpO107Rsc <> *blanks                        *SI destinatario
181300011017     c*---               MOVEL     deplna        OutO107Poc                     *linea da CAPPARIO
181400011017if  2c*---               IF        InpO107Nac <> *blanks AND                    *consegna Estero
181500011017     c*---                         InpO107Sse = 'D'                             *servizio DPD
181600011017     c*---               MOVEL     deplad        OutO107Poc                     *linea DPD
181700011017e   2c*---               ENDIF
181800011017x   1c*---               ELSE                                                   *NO destinatario
181900011017if  2c*---               IF        InpO107Sse = 'D'                             *servizio DPD
182000011017     c*---               EVAL      OutO107No1 =  %TRIM(OutO107No1) + ' ' +
182100011017     c*---                         'Scelta DPD'
182200011017e   2c*---               ENDIF
182300011017e   1c*---               ENDIF
182400000000     c*******************
182500000000     c* RIcalcolo PO ritiro --> se scelto servizio DPD occorre ricalcolare il PO di ritiro in base
182600000000     c*                         alla nazione del ritiro
182700000000     c*******************
182800000000     c                   EVAL      OutO107Pr2 = InpO107Por
182900000000if  1c                   IF        InpO107Sse = 'D' AND                         *servizio DPD
183000000000     c                             InpO107Nar <> *blanks                        *ritiro Estero
183100000000     c                   SETOFF                                           98
183200000000     c                   Z-ADD     1             i
183300000000     c     Inpo107Nar    LOOKUP    SNaz(i)                                98
183400000000if  2c                   IF        *in98                                        *NON trovata
183500000000     c                   MOVEL     SNLad(i)      OutO107Pr2
183600000000e   2c                   ENDIF
183700000000e   1c                   ENDIF
183800020320     c*******************
183900020320     c* RIcalcolo PO emissione --> Cliente senza PASSWORD: se scelto servizio DPD occorre
184000020320     c*                            ricalcolare il PO di emissione in base alla nazione del ritiro
184100020320     c*******************
184200020320     c                   EVAL      OutO107Pe2 = InpO107Poe
184300020320if  1c                   IF        InpO107Ksu > *zeros                          *SI cliente PW
184400020320x   1C                   ELSE                                                   *NO cliente PW
184500020320if  2c                   IF        InpO107Sse = 'D' AND                         *servizio DPD
184600020320     c                             InpO107Nar <> *blanks                        *ritiro Estero
184700020320     c                   MOVEL     OutO107Pr2    OutO107Pe2
184800020320e   2c                   ENDIF
184900020320e   1c                   ENDIF
185000000000     c*
185100000000     c                   ENDSR
185200000000     c*--------------------------------------------------------------------------------------------*
185300000000     c* caricamento tabelle occorrenti
185400000000     c*--------------------------------------------------------------------------------------------*
185500000000     c     cartab        BEGSR
185600000000     c*---
185700000000     c* organigramma
185800000000     c*---
185900000000     c                   CLEAR                   i                              *indice
186000000000     C                   CLEAR                   sorg
186100000000     C                   CLEAR                   sodes
186200000000     C                   CLEAR                   sofl1
186300000000     C                   CLEAR                   sopt
186400000000     C                   CLEAR                   sodpd
186500000000     c     *loval        SETLL     azorg01l
186600000000     c                   READ      azorg01l                               99
186700000000do  1c                   DOW       NOT *in99
186800000000     c                   ADD       1             i
186900000000     c                   Z-ADD     orgfil        sorg(i)
187000000000     c                   MOVEL     orgdes        sodes(i)
187100020702     c                   MOVEL     orgde3        og143
187200020702     C*
187300020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
187400020702     C                   CLEAR                   dntw
187500020702     C*
187600020702     C                   MOVEL(P)  'NTW'         kbecod
187700020702     C                   MOVEL(P)  §ogntw        kbeke1
187800020702     C     keytbe        CHAIN     tntbe01l
187900020702     C                   IF        %found(tntbe01l)
188000020702     C                   MOVEL     tbeuni        dntw
188100020702     C                   ENDIF
188200020702     C*
188300020702     C                   MOVEL     §ntwfie       sofl1(i)
188400140226     C*                  IF        §ogntw = 'DPD'
188500140226     C*                  MOVEL     'E'           sofl1(i)
188600140226     C*                  ENDIF
188700020702     C*
188800020702     c                   MOVEL     §ogntw        sopt(i)
188900020702     c                   MOVEL     §ogntw        sodpd(i)
189000000000     c                   READ      azorg01l                               99
189100000000e   1c                   ENDDO
189200000000     c*---
189300000000     c* provincie
189400000000     c*---
189500000000     c                   CLEAR                   i                              *indice
189600000000     C                   CLEAR                   dspr
189700000000     C                   CLEAR                   SPrv
189800000000     C                   CLEAR                   SPDes
189900020906     C                   Z-ADD     1             kblkut
190000000000     C                   MOVEL     'PR'          kblcod
190100000000     c     keytab        SETLL     tabel00f
190200000000     c     keytab        READE     tabel00f                               99
190300000000do  1c                   DOW       NOT *in99
190400000000if  2c                   IF        tblflg = *blanks
190500000000     c                   MOVEL     tbluni        dspr
190600000000     c                   ADD       1             i
190700000000     c                   MOVEL     tblkey        SPrv(i)
190800000000     c                   MOVEL     §Prdes        SPDes(i)
190900000000e   2c                   ENDIF
191000000000     c     keytab        READE     tabel00f                               99
191100000000e   1c                   ENDDO
191200000000     c*---
191300000000     c* nazioni
191400000000     c*---
191500000000     c                   CLEAR                   i                              *indice
191600000000     C                   CLEAR                   ds15
191700000000     C                   CLEAR                   SNaz
191800000000     C                   CLEAR                   SNDes
191900020906     C                   Z-ADD     1             kblkut
192000000000     C                   MOVEL     '15'          kblcod
192100000000     c     keytab        SETLL     tabel00f
192200000000     c     keytab        READE     tabel00f                               99
192300000000do  1c                   DOW       NOT *in99
192400000000if  2c                   IF        tblflg = *blanks
192500000000     c                   MOVEL     tbluni        ds15
192600000000     c                   ADD       1             i
192700000000     c                   MOVEL     tblkey        SNaz(i)
192800000000     c                   MOVEL     §15des        SNDes(i)
192900000000     c                   Z-ADD     §15lad        SNLad(i)
193000000000e   2c                   ENDIF
193100000000     c     keytab        READE     tabel00f                               99
193200000000e   1c                   ENDDO
193300090210     c*---
193400090210     c* Regole ritiro DPD.
193500090210     c*---
193600090210     c                   EVAL      kblkut = 1
193700090210     c                   EVAL      kblcod = '3I'
193800090210     c                   EVAL      kblkey = 'DPD'
193900090210     c     keyTab3       CHAIN     tabel00f
194000090210     c                   IF        %FOUND() AND tblFlg = *BLANK
194100090210     c                   EVAL      ds3idp = tblUni
194200090210     c                   ELSE
194300090210     c                   CLEAR                   ds3idp
194400090210     c                   ENDIF
194500000000     c*
194600000000     c                   ENDSR
194700010919     c*--------------------------------------------------------------------------------------------*
194800010919     c* operazioni finali
194900010919     c*--------------------------------------------------------------------------------------------*
195000010919     c     rutfin        BEGSR
195100010919     c*
195200010919     c* chiude pgm
195300010919     C                   CLEAR                   tisi95ds
195400010919     C                   MOVEL     'C'           I95tla
195500010919     C                   CALL      'TISI95R'                                    *controlla CAP
195600010919     C                   PARM                    tisi95ds
195700010919     c*
195800010919     c                   ENDSR
195900000000     c*--------------------------------------------------------------------------------------------*
196000000000     c* operazioni iniziali -da fare sempre-
196100000000     c*--------------------------------------------------------------------------------------------*
196200000000     c     rutinz        BEGSR
196300000000     c*
196400000000     c* reperimento data corrente
196500000000     C                   TIME                    n14                            *ora (6)+ data (8)
196600000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
196700000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
196800000000     C                   Z-ADD     n8            g08dat
196900000000     C                   Z-ADD     *zeros        g08inv
197000000000     C                   MOVEL     '0'           g08err
197100000000     c                   CALL      'XSRDA8'
197200000000     c                   PARM                    wlbda8
197300000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
197400000000     c*
197500000000     c* azzera la DS di Output e le variabili di lavro
197600000000     c                   CLEAR                   tio107ODs                      *Ds Output
197700000000     c                   MOVEL     *ALL'0'       Outo107Err                     *NO errore
197800000000     c                   MOVEL     *all'0'       SErr                           *schiera errori
197900000000     c                   MOVEL     *blanks       SMsg                           *schiera msg errori
198000000000     c                   Z-ADD     *zeros        $e                             *indice  errori
198100000000     c                   MOVEL     '0'           $err                           *controllore errore
198200000000     c*
198300000000     c* azzera le variabili "normalizzate"
198400000000     c                   CLEAR                   NInpo107Dar
198500000000     c                   CLEAR                   NInpo107Orr
198600000000     c                   CLEAR                   NInpo107AD1
198700000000     c                   CLEAR                   NInpo107AA1
198800000000     c                   CLEAR                   NInpo107AD2
198900000000     c                   CLEAR                   NInpo107AA2
199000000000     c                   CLEAR                   NInpo107Rer
199100000000     c                   CLEAR                   NInpo107Ter
199200000000     c                   CLEAR                   NInpo107Rsc
199300000000     c                   CLEAR                   NInpo107Inc
199400000000     c                   CLEAR                   NInpo107Cac
199500000000     c                   CLEAR                   NInpo107Loc
199600000000     c                   CLEAR                   NInpo107Prc
199700000000     c                   CLEAR                   NInpo107Nac
199800000000     c                   CLEAR                   NInpo107Pkg
199900000000     c                   CLEAR                   NInpo107Vlm
200000000000     c                   CLEAR                   NInpo107Bnc
200100000000     c                   CLEAR                   NInpo107Ncl
200200000000     c                   CLEAR                   NInpo107Blc
200300000000     c                   CLEAR                   NInpo107Att
200400000000     c                   CLEAR                   NInpo107Mtc
200500000000     c                   CLEAR                   NInpo107Nam
200600000000     c*
200700000000     c                   ENDSR
200800000000     c*--------------------------------------------------------------------------------------------*
200900000000     c* Operazioni iniziali -solo la prima volta-
201000000000     c*--------------------------------------------------------------------------------------------*
201100000000     C     *InzSr        BEGSR
201200000000     c*
201300000000     c* ricevimento parametri
201400000000     c     *ENTRY        PLIST
201500000000     c                   PARM                    tio107IDS
201600000000     c                   PARM                    tio107ODS
201700000000     c*
201800000000     c* chiavi di lettura
201900000000     c     keytab        KLIST                                                  tabel00f
202000000000     c                   KFLD                    kblkut                         -utente
202100000000     c                   KFLD                    kblcod                         -tabella
202200090210     c     keyTab3       KLIST                                                  tabel00f
202300090210     c                   KFLD                    kblkut                         -utente
202400090210     c                   KFLD                    kblcod                         -tabella
202500090210     c                   KFLD                    kblkey                         -elemento
202600000000     c     keytbe        KLIST                                                  *tntbe01l
202700000000     c                   KFLD                    kbecod                          -tabella
202800000000     c                   KFLD                    kbeke1                          -chiave uno
202900000000     c     ke2tbe        KLIST                                                  *tntbe02l
203000000000     c                   KFLD                    kbecod                          -tabella
203100000000     c                   KFLD                    kbelin                          -lingua
203200000000     c                   KFLD                    kbesif                          -s.informativo
203300000000     c*
203400000000     c* agiunge le librerie in lista
203500041125     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
203600041125     c*                  CALL      wpgm
203700000000     c*
203800000000     c* apre i files
203900000000     c                   OPEN      tabel00f
204000000000     c                   OPEN      tntbe01l
204100000000     c                   OPEN      tntbe02l
204200000000     c                   OPEN      azorg01l
204300000000     c*
204400000000     c* caricamento tabelle occorrenti
204500000000     c                   EXSR      cartab
204600000000     c*
204700000000     c                   ENDSR
204800060605
204900000000** -MSG- Messaggi di uscita                                                        num  cod
205000010917/ Data di ritiro errata                                                             1    1
205100000000/ Ora  pronta merce errata                                                          2    2
205200000000/ Prima   ora apertura errata                                                       3    3
205300000000/ Seconda ora apertura errata                                                       4    4
205400000000/ Errori di congruità fra gli orari di apertura indicati                            5    5
205500000000/ Peso errato                                                                       6    6
205600000000/ Volume errato                                                                     7    7
205700000000/ Numero bancali errato                                                             8    8
205800000000/ Immettere almeno una quantità fra peso, volume o bancali                          9    9
205900000000/ Numero colli errato                                                               10   A
206000000000/ Numero bilici  errato                                                             11   B
206100000000/ Numero autotreni errato                                                           12   C
206200000000/ Numero motrici errato                                                             13   D
206300000000/ Inserire i dati della persona da contattare per il ritiro                         14   E
206400011108/ Inserire se la consegna è da effettuarsi da uno o più destinatari                 15   F
206500000000/ Inserire i dati del destinatario                                                  16   G
206600000000/ Ragione sociale errata                                                            17   H
206700000000/ Indirizzo errato                                                                  18   I
206800000000/ CAP errato                                                                        19   L
206900000000/ Località errata                                                                   20   M
207000000000/ Provincia errata                                                                  21   N
207100000000/ Nazione errata                                                                    22   O
207200000000/ Dati destinatario errati                                                          23   P
207300000000/ Scegliere la modalità di consegna                                                 24   Q
207400000000/ Scegliere il network estero                                                       25   R
207500000000/ Descrizione merce obbligatoria                                                    26   S
207600010919/ L' orario attuale non consente di effettuare il ritiro in giornata                27   T
207700011108/ La consegna deve avvenire presso un SINGOLO destinatario                          28   U
207800020517/ Per consegne all'estero richiedere il pagamento da parte del mittente             29   V
207900021029/ NON inserire i dati del destinatario                                              30   Z
208000011023                                                                                    31   ;
208100011023                                                                                    32   ,
208200011023                                                                                    33   .
208300011023                                                                                    34   :
208400011023                                                                                    35   -
208500011023                                                                                    36   _
208600011023                                                                                    37   |
208700011023                                                                                    38   \
208800011023                                                                                    39   !
208900011023/ Tornare indietro e RIcontrollare i dati immessi                                   40   £
