000100060609     H dftactgrp(*NO) actgrp(*CALLER) bnddir('TIS')
000200090211     h DATEDIT(*DMY) DECEDIT(',')
000300090211     h*--------------------------------------------------------------------------------------------*
000400000000     h* Description: ORM - Controlli immissione multipla
000500000000     h* Richiamato dal TIO120S su AS777 con OPCODE = CHECKIMM
000600000000     f*--------------------------------------------------------------------------------------------*
000700000000     f* Data base
000800000000     f*--------------------------------------------------------------------------------------------*
000900020321     h*---
001000020321     h* Clienti abilitati
001100020321     h*---
001200020321     ftivss02l  IF   E           K DISK    USROPN
001300020321     h*---
001400020321     h* Clienti ritiro
001500020321     h*---
001600020321     ffnacr01l  IF   E           K DISK    USROPN
001700000000     f*---
001800000000     f* Tabelle
001900000000     f*---
002000000000     ftabel00f  IF   E           K DISK    USROPN
002100000000     f*---
002200000000     f* Tabelle -NEW-
002300000000     f*---
002400000000     ftntbe01l  IF   E           K DISK    USROPN
002500000000     ftntbe02l  IF   E           K DISK    USROPN
002600000000     f                                     RENAME(tntbe000:tntbe2)
002700000000     f*---
002800000000     f* Organigramma
002900000000     f*---
003000000000     fazorg01l  IF   E           K DISK    USROPN
003100060605
003200060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
003300060612     D  msgId                         7A   CONST
003400060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003500060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003600060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003700060612     D                                     VARYING
003800060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003900060612     D                                     VARYING
004000060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
004100060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
004200060612
004300060605     d*--------------------------------------------------------------------------------------------*
004400000000     d* Data structure
004500000000     d*--------------------------------------------------------------------------------------------*
004600040921     D                SDS
004700040921     D  PgmLibrary            81     90
004800000000     d*---
004900000000     d* Costanti
005000000000     d*---
005100000000     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005200000000     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005300000000     d $Dim            C                   CONST(200)                           *n° clienti da memo
005400120829     D IRLANDA         C                   'IRL'
005500120829     D EIRE            C                   'EIRE'
005600120829     d*---
005700000000     d* Schiere a tempo di compilazione
005800000000     d*---
005900000000     d msg             S             80    DIM(40) CTDATA PERRCD(1)             *messaggi di uscita
006000000000     d*---
006100000000     d* Variabili riferite al data base
006200000000     d*---
006300000000     d kblkut          S                   LIKE(tblkut)                         *lettura tabel00f
006400000000     d kblcod          S                   LIKE(tblcod)
006500090211     d kblkey          S                   LIKE(tblkey)
006600090211     d kbecod          S                   LIKE(tbecod)                         *lettura tntbe01l
006700000000     d kbeke1          S                   LIKE(tbeke1)
006800000000     d kbeke2          S                   LIKE(tbeke2)
006900000000     d kbelin          S                   LIKE(tbelin)
007000000000     d kbesif          S                   LIKE(tbesif)
007100020321     d kcrcro          S                   LIKE(acrcro)                         *lettura fnacr00f
007200020321     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
007300020321     d kssisv          S                   LIKE(vssisv)
007400020327     d wacr            S                   LIKE(acrcro)                         *di lavoro
007500020327     d deprsr          S                   LIKE(acrrsr)
007600020327     d depinr          S                   LIKE(acrinr)
007700020327     d depcar          S                   LIKE(acrcar)
007800020327     d deplor          S                   LIKE(acrlor)
007900020327     d depprr          S                   LIKE(acrprr)
008000020327     d depnar          S                   LIKE(acrnar)
008100020325     d deplnaR         S                   LIKE(o95lna)                         *depositi
008200000000     d deplad          S                   LIKE(o95lna)
008300020325     d NInpO121Cde     S                   LIKE(acrcro)                         *cambi"normalizzati"
008400020325     d NInpO121Rsr     S                   LIKE(InpO121Rsr)
008500020325     d NInpO121Inr     S                   LIKE(InpO121Inr)
008600020325     d NInpO121Nar     S                   LIKE(InpO121Nar)
008700020325     d NInpO121Lor     S                   LIKE(InpO121Lor)
008800020325     d NInpO121Car     S                   LIKE(InpO121Car)
008900020325     d NInpO121Prr     S                   LIKE(InpO121Prr)
009000020325     d NInpO121Dar     S                   LIKE(InpO121Dar)
009100020328     d NInpo121Rer     S                   LIKE(Inpo121Rer)
009200020328     d NInpo121Ter     S                   LIKE(Inpo121Ter)
009300020325     d NInpO121Orr     S                   LIKE(InpO121Orr)
009400020325     d NInpO121AD1     S                   LIKE(InpO121AD1)
009500020325     d NInpO121AA1     S                   LIKE(InpO121AA1)
009600020325     d NInpO121AD2     S                   LIKE(InpO121AD2)
009700020325     d NInpO121AA2     S                   LIKE(InpO121AA2)
009800020325     d NInpO121Rsc     S                   LIKE(InpO121Rsc)
009900020325     d NInpO121Inc     S                   LIKE(InpO121Inc)
010000020325     d NInpO121Nac     S                   LIKE(InpO121Nac)
010100020325     d NInpO121Loc     S                   LIKE(InpO121Loc)
010200020325     d NInpO121Cac     S                   LIKE(InpO121Cac)
010300020325     d NInpO121Prc     S                   LIKE(InpO121Prc)
010400000000     d*---
010500000000     d* Variabili di controlli
010600000000     d*---
010700000000     d $e              S              2  0                                      *indice errori Out
010800000000     d $err            S              1                                         *controllore errore
010900020325     d $OkCde          S              1                                         *validità C.Ritiro
011000000000     d $oknum          S              1                                         *validità numerica
011100060605     D msgId...
011200060605     D                 S              7A
011300060605     D msgDta...
011400060605     D                 S            512A
011500060605     D msg1...
011600060605     D                 S            644A
011700060605     D secLvl...
011800060605     D                 S           3512A
011900060605     D rtnCode...
012000060605     D                 S             10A
012100060605     D esito15...
012200060605     D                 S             15P 0
012300060605     d*---
012400000000     d* Variabili NON riferite al data base
012500000000     d*---
012600000000     d i               S              3  0                                      *indici
012700000000     d ii              S              3  0
012800000000     d n14             S             14  0                                      *Numerico 14
012900000000     d n2              S              2  0                                      *Numerico 2
013000000000     d n3              S              3  0                                      *Numerico 3
013100000000     d n8              S              8  0                                      *Numerico 8
013200000000     d n71             S              7  1                                      *Numerico 7,1
013300000000     d a1              S              1                                         *Alfabetico 1
013400000000     d a3              S              3                                         *Alfabetico 3
013500000000     d a7              S              7                                         *Alfabetico 7
013600000000     d a5              S              5                                         *Alfabetico 5
013700000000     d datcor          S              8  0                                      *data corrente a/m/g
013800000000     d oracor          S              6  0                                      *ora corrente
013900000000     d dardep          S              8  0                                      *dep.data pr.merce
014000020321     d depcli          S              7  0                                      *depositi
014100000000     d werr            S              1                                         *errore generico
014200000000     d wimpmsg         S              1                                         *msg da impostare
014300000000     d wmsg            S             80                                         *msg impostato
014400020325     d tmsg            S              1                                         *tipo msg impostato
014500020321     d wcli            S              7  0                                      *di lavoro
014600000000     d wpgm            S             20                                         *programma
014700000000     d wdat            S              8  0                                      *data (aaaammgg)
014800000000     d wdate           S             10                                         *data (gg/mm/aaaa)
014900000000     d wora            S              4  0                                      *ora  (hhmm)
015000000000     d worae           S              5                                         *ora  (hh:mm)
015100040921     D CmdExcStr       S             50
015200040921     D CmdExcLen       S             15  5
015300040921     D                                     INZ(%SIZE(CmdExcStr))
015400000000     d*---
015500000000     d* Schiere
015600000000     d*---
015700000000     d SErr            S              1    DIM(10)                              *errori          Out
015800000000     d SMsg            S             80    DIM(10)                              *messaggi errori Out
015900020321      * clienti figli
016000020321     d sksc7           S              7  0 DIM(500)                             *clienti figli
016100020321      * clienti ritiro
016200020321     d SCro            S             10S 0 Dim($Dim)                            *clienti ritiro
016300000000      * tabella filiali
016400000000     d SOrg            S              3  0 DIM(300)                             *filiale
016500000000     d SOdes           S             30    DIM(300)                             *descrizione filiale
016600000000     d SOfl1           S              1    DIM(300)                             *flag italia/estero
016700000000     d SOpt            S              1    DIM(300)                             *flag poste/no poste
016800000000     d SOdpd           S              1    DIM(300)                             *flag DPD/no DPD
016900000000      * tabella provincie
017000000000     d SPrv            S              2    DIM(200)                             *provincia
017100000000     d SPDes           S             20    DIM(200)                             *descriz.provincia
017200000000      * tabella nazioni
017300000000     d SNaz            S              3    DIM(500)                             *nazione
017400000000     d SNDes           S             25    DIM(500)                             *descrizione nazione
017500000000     d SNLad           S              3  0 DIM(500)                             *l.arrivo DPD
017600000000     d*---
017700000000     d* Parametri I/O
017800000000     d*---
017900020321     d tio121IDs     E Ds                  PREFIX(Inp)                          *Input
018000020321     d tio121ODs     E Ds                  PREFIX(Out)                          *Output
018100020327     d tio110IDS     E ds                  PREFIX(Inp)                          *Inp scrittura ORM
018200020327     d tio110ODS     E ds                  PREFIX(Out)                          *Out scrittura ORM
018300000000     d*---
018400000000     d* Ds
018500000000     d*---
018600000000     d* controllo data
018700000000     d wlbda8          DS                  INZ                                  *controlla data
018800000000     d  g08dat                        8  0                                       -data dritta
018900000000     d  g08inv                        8  0                                       -data invertita
019000000000     d  g08err                        1                                          -errore
019100000000     d  g08tgi                        5  0                                       -giorni fra date
019200000000      * scomposizione data generica
019300000000     d dsdat           DS                  INZ                                  *data
019400000000     d  dsann                         4  0                                       -anno
019500000000     d  dsmes                         2  0                                       -mese
019600000000     d  dsgio                         2  0                                       -giorno
019700000000      * scomposizione data generica (anno + mese/giorno)
019800000000     d                 DS                  INZ                                  *data aa + mm/gg
019900000000     d  dsannB                 1      4  0                                       -anno
020000000000     d  dsmgsB                 5      8  0                                       -mese/giorno
020100000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
020200000000      * composizione data generica editata
020300000000     d dsdate          DS                  INZ                                  *data
020400000000     d  dsgioe                        2  0                                       -giorno
020500000000     d  dsvd1                         1                                          -'/'
020600000000     d  dsmese                        2  0                                       -mese
020700000000     d  dsvd2                         1                                          -'/'
020800000000     d  dsanne                        4  0                                       -anno
020900000000      * scomposizione ora generica
021000000000     d dsora           DS                  INZ                                  *ora
021100000000     d  dshh                          2  0                                       -ore
021200000000     d  dsmm                          2  0                                       -minuti
021300000000      * composizione ora generica editata
021400000000     d dsorae          DS                  INZ                                  *ora
021500000000     d  dshhe                         2  0                                       -ore
021600000000     d  dsvo1                         1                                          -':'
021700000000     d  dsmme                         2  0                                       -minuti
021800000000     d* controlla cap
021900000000     d tisi95ds      E DS                                                       *parametri chiamata
022000081119     D tisi97ds      E DS                  INZ
022100081119     D fnlv12ds      E DS                  INZ
022200081119     d* reperimento clienti da cliente unificante
022300020325     d tibs10ds      E DS                                                       *parametri chiamata
022400020325     d  sksc11                21   5520  0 DIM(500)                              -schiera clienti
022500000000      * lettura campo orgde3 di AZORG00F
022600000000     d og143         E DS
022700000000      * DS Provincie
022800000000     d dspr          E DS
022900000000      * DS Nazioni
023000000000     d ds15          E DS
023100090211     d ds3idp        E DS                  QUALIFIED
023200090211     d                                     INZ
023300090211
023400041126     D Esito           S              5I 0
023500041126     D Msg708          S             80
023600041126
023700000000     c*--------------------------------------------------------------------------------------------*
023800000000     c* Main lines
023900000000     c*--------------------------------------------------------------------------------------------*
024000000000     c*
024100000000     c* operazioni iniziali -da fare sempre-
024200000000     c                   EXSR      rutinz
024300020321     c*
024400020321     C* carica in schiere i clienti ritiro dell'utente STRATEGI
024500020321     c                   EXSR      CarAcr
024600020321     c*
024700020325     c* controlla il codice cliente destinatario (fra i codici cliente ritiro)
024800020325     c                   EXSR      ChkInpCde
024900020321     c*
025000020321     c* controlla il nominativo ritiro
025100020321     c                   EXSR      ChkInpRsr
025200020321     c*
025300020321     c* controlla l' indirizzo ritiro
025400020321     c                   EXSR      ChkInpInr
025500020321     c*
025600020321     c* controlla la nazione ritiro
025700020321     c                   EXSR      ChkInpNar
025800020321     c*
025900020321     c* controlla il CAP ritiro
026000020321     c                   EXSR      ChkInpCar
026100020321     c*
026200020321     c* controlla la località ritiro
026300020321     c                   EXSR      ChkInpLor
026400020321     c*
026500020321     c* controlla la provincia ritiro
026600020321     c                   EXSR      ChkInpPrr
026700020321     c*
026800020321     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione ritiro
026900020321     c                   EXSR      ChkInpICKPNr
027000020321     c*
027100020321     c* controlla il peso
027200020321     c                   EXSR      ChkInpPkg
027300020321     c*
027400020321     c* controlla il numero colli
027500020321     c                   EXSR      ChkInpNcl
027600020321     c*
027700020321     c* controlla la data pronta merce
027800041126     c*                  EXSR      ChkInpDar
027900020321     c*
028000020321     c* controlla l' ora pronta merce
028100041126     c*                  EXSR      ChkInpOrr
028200020328     c*
028300041126     c* Controlla data e ora pronta merce.
028400041126     c                   EXSR      ChkDaROrR
028500041126     c*
028600020328     c* controlla la persona da contattare e il suo numero di telefono
028700020328     c                   EXSR      ChkInpRer
028800020321     c*
028900020321     c* controlla il pagamento
029000020321     c                   EXSR      ChkInpPag
029100020321     c*
029200020321     c* controlla la quantita merce
029300020321     c                   EXSR      ChkInpQta
029400000000     c*
029500000000     c* controlla gli orari di apertura UNO
029600000000     c                   EXSR      ChkInpOA1
029700000000     c*
029800000000     c* controlla gli orari di apertura DUE
029900000000     c                   EXSR      ChkInpOA2
030000000000     c*
030100000000     c* controlla gli orari di apertura UNO e DUE
030200000000     c                   EXSR      ChkInpOA1OA2
030300000000     c*
030400000000     c* controlla se ritiro singolo / multiplo
030500000000     c                   EXSR      ChkInpUPD
030600000000     c*
030700000000     c* controlla il destinatario
030800000000     c                   EXSR      ChkInpDes
030900020321     c*
031000000000     c* controlla il fermo deposito
031100000000     c                   EXSR      ChkInpFfd
031200000000     c*
031300000000     c* controlla la scelta del network estero
031400000000     c                   EXSR      ChkInpSse
031500000000     c*
031600020327     c* se non ci sono errori
031700000000if  1c                   IF        $err = '0'                                   *NO errori
031800020327     c*
031900020327     c* imposta i campi della DS di Output
032000000000     c                   EXSR      ImpDsOut
032100020327     c*
032200020327     c* lancia la srittura dell'ORM
032300020327     C                   EXSR      LanWriVpi
032400020327e   1C                   ENDIF                                                  *SI errori
032500020327     c*
032600020327     c* se ci sono errori, porta i relativi messaggi in Output
032700020327if  1c                   IF        $err <> '0'                                  *SI errori
032800020321     c                   MOVEA     SErr          Outo121Err                     *codice
032900020321     c                   MOVEA     SMsg          Outo121Msg                     *messaggio
033000000000e   1c                   ENDIF
033100000000     c*
033200000000     c* operazioni finali
033300000000     c                   EXSR      rutfin
033400000000     c*
033500000000     C                   RETURN
033600020321     c*--------------------------------------------------------------------------------------------*
033700020325     c* controlla il codice cliente destinatario (fra i codici cliente ritiro)
033800020321     c*--------------------------------------------------------------------------------------------*
033900020325     c     ChkInpCde     BEGSR
034000020321     c*
034100020321     c* reimposta le variabili di lavoro
034200020321     c                   MOVEL     '0'           werr                           *NO errore
034300020325     c                   CLEAR                   NInpO121Cde                    *campo"normalizzato"
034400020325     c                   MOVEL     'N'           $OkCde                         *validità
034500020321     c*
034600020321     c* controlla il dato solo se immesso
034700020321do  1c                   DO
034800020325if  2c                   IF        InpO121Cde <> *blanks AND
034900020325     c                             InpO121Cde > *zeros
035000020321     c*
035100020325     c* controlla se il codice cliente destinatario è tutto un numero
035200020321     c                   SETOFF                                       98
035300020325     c                   TESTN                   InpO121Cde           98
035400020321if  3c                   IF        *in98                                        *da TESTN è numero
035500020325     c                   MOVE      InpO121Cde    a1                             *ultimo carattere
035600020321if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
035700020325     c                   MOVEL     'S'           $OkCde                         *valido
035800020321e   4c                   ENDIF
035900020321e   3c                   ENDIF
036000020325if  3c                   IF        $OkCde = 'N'                                 *NON valido
036100020321     c                   MOVEL     '1'           werr                           *errore Output
036200020321     c                   LEAVE
036300020321e   3c                   ENDIF
036400020321     c*
036500020321     c* controlla se il cliente ritiro appartiene a quelli del codice STRATEGI di entrata
036600020321if  3c                   IF        werr = '0'                                   *ancora valido
036700020325     c                   MOVEL     InpO121Cde    NInpO121Cde                    *campo"normalizzato"
036800020321     c                   SETOFF                                           98
036900020321     c                   Z-ADD     1             i
037000020325     c     NInpO121Cde   LOOKUP    SCro(i)                                98
037100020321if  4c                   IF        NOT *in98                                    *NON trovato
037200020321     c                   MOVEL     '1'           werr                           *errore Output
037300020321     c                   LEAVE
037400020321     c*
037500020321     c* legge e controlla il cliente ritiro in anagrafica
037600020321x   4c                   ELSE                                                   *trovato
037700020327     c                   MOVEL     NInpO121Cde   wacr
037800020321     c                   EXSR      LetAcrCro
037900020321e   4c                   ENDIF
038000020321e   3c                   ENDIF
038100020321e   2c                   ENDIF
038200020321     C*
038300020321e   1C                   ENDDO
038400020321     c*
038500020321     c* se errore, memorizza il codice errore nella schiera errori
038600020321if  1c                   IF        werr = '1'
038700020321if  2c                   IF        $e < 10                                      *non + di 10 errori
038800020321     c                   ADD       1             $e
038900020321     c                   MOVEL     '1'           Serr($e)                       *aggiunge errore
039000060605     c*                  MOVEL     msg(1)        Smsg($e)                       *messaggio
039100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0066'
039200060609     C                             :inpO121lang)
039300060609     c                   MOVEL     '1'           $err                           *controllore errore
039400020321e   2c                   ENDIF
039500020321e   1c                   ENDIF
039600020321     c*
039700020321     c                   ENDSR
039800020321     c*--------------------------------------------------------------------------------------------*
039900020321     c* legge e controlla il cliente ritiro in anagrafica
040000020321     c*--------------------------------------------------------------------------------------------*
040100020321     c     LetAcrCro     BEGSR
040200020321     c*
040300020327     c* azzera le variabili di output
040400020327     c                   CLEAR                   deprsr
040500020327     c                   CLEAR                   depinr
040600020327     c                   CLEAR                   depcar
040700020327     c                   CLEAR                   deplor
040800020327     c                   CLEAR                   depprr
040900020327     c                   CLEAR                   depnar
041000020327     c*
041100020327     c* controlla esistenza codice
041200020327     c                   MOVEL     NInpO121Cde   wacr
041300020411     c                   MOVEL     NInpO121Cde   kcrcro
041400020321     c     keyacr        CHAIN     fnacr01l                           98
041500020321if  1c                   IF        NOT *in98        AND                         *esiste
041600020321     c                             acratb = *blanks AND                         *NO annullati
041700020321     c                             acrtcr <> 'M'                                *NO infrequenti
041800020327     c                   EVAL      deprsr = ACRRSR
041900020327     c                   EVAL      depinr = ACRINR
042000020327     c                   EVAL      depcar = ACRCAR
042100020327     c                   EVAL      deplor = ACRLOR
042200020327     c                   EVAL      depprr = ACRPRR
042300020327     c                   EVAL      depnar = ACRNAR
042400020327x   1c                   ELSE                                                   *NON esiste
042500020321     c                   MOVEL     '1'           werr                           *errore
042600020321e   1c                   ENDIF
042700020321     c*
042800020321     c                   ENDSR
042900020321     c*--------------------------------------------------------------------------------------------*
043000020321     c* controlla il nominativo ritiro
043100020321     c*--------------------------------------------------------------------------------------------*
043200020321     c     ChkInpRsr     BEGSR
043300020321     c*
043400020321     c* reimposta le variabili di lavoro
043500020321     c                   MOVEL     '0'           werr                           *NO errore
043600020325     c                   CLEAR                   NInpO121Rsr                    *campo"normalizzato"
043700020325     c*
043800020325     c* campo obbligatorio
043900020321do  1C                   DO
044000020325if  2c                   IF        InpO121Rsr = *blanks
044100020321     c                   MOVEL     '1'           werr                           *SI errore
044200020321     c                   LEAVE
044300020321e   2c                   ENDIF
044400020321     c*
044500020321e   1c                   ENDDO
044600020321     c*
044700020321     c* trasforma il campo immesso da minuscolo a maiuscolo
044800020321     c     minu:maiu     XLATE     InpO121Rsr    NInpO121Rsr                    *Minus -> Maiuscolo
044900020321     C*
045000020321     c*
045100020321     c* se errore, memorizza il codice errore nella schiera errori
045200020321if  1c                   IF        werr = '1'
045300020321if  2c                   IF        $e < 10                                      *non + di 10 errori
045400020321     c                   ADD       1             $e
045500020321     c                   MOVEL     '2'           Serr($e)                       *aggiunge errore
045600060605     c*                  MOVEL     msg(2)        Smsg($e)                       *messaggio
045700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0482'
045800060609     C                             :inpO121lang)
045900060609     c                   MOVEL     '1'           $err                           *controllore errore
046000020321e   2c                   ENDIF
046100020321e   1c                   ENDIF
046200020321     c*
046300020321     c                   ENDSR
046400020321     c*--------------------------------------------------------------------------------------------*
046500020321     c* controlla l' indirizzo ritiro
046600020321     c*--------------------------------------------------------------------------------------------*
046700020321     c     ChkInpInr     BEGSR
046800020321     c*
046900020321     c* reimposta le variabili di lavoro
047000020321     c                   MOVEL     '0'           werr                           *NO errore
047100020321     c                   CLEAR                   NInpO121Inr                    *campo"normalizzato"
047200020321     c*
047300020321     c* campo obbligatorio se non inerito il codice cliente ritiro
047400020321do  1C                   DO
047500020325if  2c                   IF        InpO121inr = *blanks
047600020321     c                   MOVEL     '1'           werr                           *SI errore
047700020321     c                   LEAVE
047800020321e   2c                   ENDIF
047900020321     C*
048000020321e   1c                   ENDDO
048100020321     c*
048200020321     c* trasforma il campo immesso da minuscolo a maiuscolo
048300020321     c     minu:maiu     XLATE     InpO121Inr    NInpO121Inr                    *Minus -> Maiuscolo
048400020321     c*
048500020321     c* se errore, memorizza il codice errore nella schiera errori
048600020321if  1c                   IF        werr = '1'
048700020321if  2c                   IF        $e < 10                                      *non + di 10 errori
048800020321     c                   ADD       1             $e
048900020321     c                   MOVEL     '3'           Serr($e)                       *aggiunge errore
049000060605     c*                  MOVEL     msg(3)        Smsg($e)                       *messaggio
049100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0283'
049200060609     C                             :inpO121lang)
049300060609     c                   MOVEL     '1'           $err                           *controllore errore
049400020321e   2c                   ENDIF
049500020321e   1c                   ENDIF
049600020321     c*
049700020321     c                   ENDSR
049800020321     c*--------------------------------------------------------------------------------------------*
049900020321     c* controlla il CAP ritiro
050000020321     c*--------------------------------------------------------------------------------------------*
050100020321     c     ChkInpCar     BEGSR
050200020321     c*
050300020321     c* reimposta le variabili di lavoro
050400020321     c                   MOVEL     '0'           werr                           *NO errore
050500020321     c                   CLEAR                   NInpO121Car                    *campo"normalizzato"
050600020321     c*
050700020321     c* campo obbligatorio se non inerito il codice cliente ritiro
050800020321do  1C                   DO
050900020325if  2c                   IF        InpO121Car = *blanks
051000020321     c                   MOVEL     '1'           werr                           *SI errore
051100020321     c                   LEAVE
051200020321e   2c                   ENDIF
051300020321     C*
051400020321e   1c                   ENDDO
051500020321     c*
051600020321     c* trasforma il campo immesso da minuscolo a maiuscolo
051700020321     c     minu:maiu     XLATE     InpO121Car    NInpO121Car                    *Minus -> Maiuscolo
051800020321     c*
051900020321     c* se errore, memorizza il codice errore nella schiera errori
052000020321if  1c                   IF        werr = '1'
052100020321if  2c                   IF        $e < 10                                      *non + di 10 errori
052200020321     c                   ADD       1             $e
052300020321     c                   MOVEL     '4'           Serr($e)                       *aggiunge errore
052400060605     c*                  MOVEL     msg(4)        Smsg($e)                       *messaggio
052500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0053'
052600060609     C                             :inpO121lang)
052700060609     c                   MOVEL     '1'           $err                           *controllore errore
052800020321e   2c                   ENDIF
052900020321e   1c                   ENDIF
053000020321     c*
053100020321     c                   ENDSR
053200020321     c*--------------------------------------------------------------------------------------------*
053300020321     c* controlla la località ritiro
053400020321     c*--------------------------------------------------------------------------------------------*
053500020321     c     ChkInpLor     BEGSR
053600020321     c*
053700020321     c* reimposta le variabili di lavoro
053800020321     c                   MOVEL     '0'           werr                           *NO errore
053900020321     c                   CLEAR                   NInpO121Lor                    *campo"normalizzato"
054000020321     c*
054100020321     c* campo obbligatorio se non inerito il codice cliente ritiro
054200020321do  1C                   DO
054300020325if  2c                   IF        InpO121Lor = *blanks
054400020321     c                   MOVEL     '1'           werr                           *SI errore
054500020321     c                   LEAVE
054600020321e   2c                   ENDIF
054700020321     C*
054800020321e   1c                   ENDDO
054900020321     c*
055000020321     c* trasforma il campo immesso da minuscolo a maiuscolo
055100020321     c     minu:maiu     XLATE     InpO121Lor    NInpO121Lor                    *Minus -> Maiuscolo
055200020321     c*
055300020321     c* se errore, memorizza il codice errore nella schiera errori
055400020321if  1c                   IF        werr = '1'
055500020321if  2c                   IF        $e < 10                                      *non + di 10 errori
055600020321     c                   ADD       1             $e
055700020321     c                   MOVEL     '5'           Serr($e)                       *aggiunge errore
055800060605     c*                  MOVEL     msg(5)        Smsg($e)                       *messaggio
055900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0347'
056000060609     C                             :inpO121lang)
056100060609     c                   MOVEL     '1'           $err                           *controllore errore
056200020321e   2c                   ENDIF
056300020321e   1c                   ENDIF
056400020321     c*
056500020321     c                   ENDSR
056600020321     c*--------------------------------------------------------------------------------------------*
056700020321     c* controlla la provincia ritiro
056800020321     c*--------------------------------------------------------------------------------------------*
056900020321     c     ChkInpPrr     BEGSR
057000020321     c*
057100020321     c* reimposta le variabili di lavoro
057200020321     c                   MOVEL     '0'           werr                           *NO errore
057300020321     c                   CLEAR                   NInpO121Prr                    *campo"normalizzato"
057400020321     c*
057500020321     c* controlla se il dato deve essere immesso oppure no
057600020321do  1c                   DO
057700020321     c*
057800020321     c* trasforma il campo immesso da minuscolo a maiuscolo
057900020321     c     minu:maiu     XLATE     InpO121Prr    NInpO121Prr                    *Minus -> Maiuscolo
058000020321     C*
058100020321     c* controlla esistenza codice in anagrafica
058200020321if  2c                   IF        NInpO121Prr <> *blanks                       *se inserito
058300020321     c                   SETOFF                                           98
058400020321     c                   Z-ADD     1             i
058500020321     c     NInpO121Prr   LOOKUP    SPrv(i)                                98
058600020321if  3c                   IF        NOT *in98                                    *NON trovata
058700020321     c                   MOVEL     '1'           werr                           *SI errore
058800020321     c                   LEAVE
058900020321e   3c                   ENDIF
059000020321e   2c                   ENDIF
059100020321     c*
059200020321e   1C                   ENDDO
059300020321     c*
059400020321     c* se errore, memorizza il codice errore nella schiera errori
059500020321if  1c                   IF        werr = '1'
059600020321if  2c                   IF        $e < 10                                      *non + di 10 errori
059700020321     c                   ADD       1             $e
059800020321     c                   MOVEL     '6'           Serr($e)                       *aggiunge errore
059900060605     c*                  MOVEL     msg(6)        Smsg($e)                       *messaggio
060000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0466'
060100060609     C                             :inpO121lang)
060200060609     c                   MOVEL     '1'           $err                           *controllore errore
060300020321e   2c                   ENDIF
060400020321e   1c                   ENDIF
060500020321     c*
060600020321     c                   ENDSR
060700020321     c*--------------------------------------------------------------------------------------------*
060800020321     c* controlla la nazione ritiro
060900020321     c*--------------------------------------------------------------------------------------------*
061000020321     c     ChkInpNar     BEGSR
061100020321     c*
061200020321     c* reimposta le variabili di lavoro
061300020321     c                   MOVEL     '0'           werr                           *NO errore
061400020321     c                   CLEAR                   NInpO121Nar                    *campo"normalizzato"
061500020321     c*
061600020321     c* controlla se il dato deve essere immesso oppure no
061700020321do  1c                   DO
061800020321     c*
061900020321     c* trasforma il campo immesso da minuscolo a maiuscolo
062000020321     c     minu:maiu     XLATE     InpO121Nar    NInpO121Nar                    *Minus -> Maiuscolo
062100020321     c*
062200020321     c* controlla esistenza codice in anagrafica
062300020321if  2c                   IF        NInpO121Nar <> *blanks                       *se inserito
062400020321     c                   SETOFF                                           98
062500020321     c                   Z-ADD     1             i
062600020321     c     NInpO121Nar   LOOKUP    SNaz(i)                                98
062700020321if  3c                   IF        NOT *in98                                    *NON trovata
062800020321     c                   MOVEL     '1'           werr                           *SI errore
062900020321     c                   LEAVE
063000020321e   3c                   ENDIF
063100020321e   2c                   ENDIF
063200020321     c*
063300020321e   1C                   ENDDO
063400020321     c*
063500020321     c* se errore, memorizza il codice errore nella schiera errori
063600020321if  1c                   IF        werr = '1'
063700020321if  2c                   IF        $e < 10                                      *non + di 10 errori
063800020321     c                   ADD       1             $e
063900020321     c                   MOVEL     '7'           Serr($e)                       *aggiunge errore
064000060605     c*                  MOVEL     msg(7)        Smsg($e)                       *messaggio
064100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0386'
064200060609     C                             :inpO121lang)
064300060609     c                   MOVEL     '1'           $err                           *controllore errore
064400020321e   2c                   ENDIF
064500020321e   1c                   ENDIF
064600020321     c*
064700020321     c                   ENDSR
064800020321     c*--------------------------------------------------------------------------------------------*
064900020321     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione ritiro
065000020321     c*--------------------------------------------------------------------------------------------*
065100020321     c     ChkInpICKPNr  BEGSR
065200020321     c*
065300020321     c* reimposta le variabili di lavoro
065400020321     c                   MOVEL     '0'           werr                           *NO errore
065500020325     c                   Z-ADD     *zeros        deplnaR                        *deposito lna ritiro
065600020321     c*
065700020321     c* controlla se il dato deve essere controllato oppure no
065800020321do  1c                   DO
065900020321     c*
066000020321     c* controlla esattezza dati ritiro
066100020321     C                   CLEAR                   tisi95ds
066200020321     C                   MOVEL     ' '           I95tla
066300020321     C                   MOVEL     '7'           I95tcn
066400020321     C                   MOVEL     NInpO121Nar   I95nar
066500020321     C                   MOVEL     NInpO121Car   I95cap
066600020321     C                   MOVEL     NInpO121Lor   I95loc
066700020321     C                   MOVEL     NInpO121Prr   I95prv
066800020321     C                   MOVEL     NInpO121Inr   I95ind
066900020321     C                   Z-ADD     datcor        I95dat
067000120829     C                   IF        i95nar = IRLANDA
067100120829     C                   EVAL      i95cap = EIRE
067200120829     C                   ENDIF
067300120829     C                   CALL      'TISI95R'
067400020321     C                   PARM                    tisi95ds
067500020321     c*
067600020321     c* se ritorna errore o dato non affidabile imposta errore
067700020325if  2c                   IF        NInpO121Nar = *blanks  AND                   *ITALIA
067800020321     c                             O95Lia <> '3'          OR                    *affidabilità 3
067900020321     c                             NInpO121Nar <> *blanks AND                   *ESTERO
068000020321     c                             O95Lia <> '2'                                *affidabilità 2
068100020321     c                   MOVEL     '1'           werr                           *SI errore
068200020321     c                   LEAVE
068300020325x   2c                   ELSE
068400020325     c                   Z-ADD     O95Lna        depLnaR                        *linea arrivo ritiro
068500020325e   2c                   ENDIF
068600020321     c*
068700020321e   1C                   ENDDO
068800020321     c*
068900020321     c* se errore, memorizza il codice errore nella schiera errori
069000020321if  1c                   IF        werr = '1'
069100020321if  2c                   IF        $e < 10                                      *non + di 10 errori
069200020321     c                   ADD       1             $e
069300020321     c                   MOVEL     '8'           Serr($e)                       *aggiunge errore
069400060605     c*                  MOVEL     msg(8)        Smsg($e)                       *messaggio
069500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0054'
069600060609     C                             :inpO121lang)
069700060609     c                   MOVEL     '1'           $err                           *controllore errore
069800020321e   2c                   ENDIF
069900020321e   1c                   ENDIF
070000020321     C*
070100020321     C                   ENDSR
070200020321     c*--------------------------------------------------------------------------------------------*
070300020321     c* controlla il peso
070400020321     c*--------------------------------------------------------------------------------------------*
070500020321     c     ChkInpPkg     BEGSR
070600020321     c*
070700020321     c* reimposta le variabili di lavoro
070800020321     c                   MOVEL     '0'           werr                           *NO errore
070900020321     c                   CLEAR                   NInpo121Pkg       7 1          *campi"normalizzati"
071000020321     c                   CLEAR                   a7
071100020321     c*
071200020321     c* controlla il campo solo se immesso
071300020321if  1c                   IF        Inpo121Pkg <> *blanks
071400020321     c     ' ' :'0'      XLATE     Inpo121Pkg    a7
071500020321do  2c                   DO
071600020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
071700020321     c                   SETOFF                                       98
071800020321     c                   TESTN                   a7                   98
071900020321if  3c                   IF        *in98                                        *da TESTN è numero
072000020321     c                   MOVE      a7            a1                             *ultimo carattere
072100020321if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
072200020321     c                   MOVEL     'S'           $OkNum                         *valido
072300020321e   4c                   ENDIF
072400020321e   3c                   ENDIF
072500020321if  3c                   IF        $OkNum = 'N'                                 *NON valido
072600020321     c                   MOVEL     '1'           werr                           *errore Output
072700020321     c                   LEAVE
072800020321e   3c                   ENDIF
072900020321     c*
073000020321     c* sposta il campo in quello "normalizzato"
073100020321     c                   MOVEL     a7            NInpo121Pkg                    *campo"normalizzato"
073200020321e   2c                   ENDDO
073300020321e   1c                   ENDIF
073400020321     c*
073500020321     c* se errore, memorizza il codice errore nella schiera errori
073600020321if  1c                   IF        werr = '1'
073700020321if  2c                   IF        $e < 10                                      *non + di 10 errori
073800020321     c                   ADD       1             $e
073900020321     c                   MOVEL     '9'           Serr($e)                       *aggiunge errore
074000060605     c*                  MOVEL     msg(9)        Smsg($e)                       *messaggio
074100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0453'
074200060609     C                             :inpO121lang)
074300060609     c                   MOVEL     '1'           $err                           *controllore errore
074400020321e   2c                   ENDIF
074500020321e   1c                   ENDIF
074600090211     ** Se richiesto il ritiro con DPD
074700090211     ** il peso diventa obbligatorio e deve essere inferiore al limite.
074800090211     C                   IF        wErr = '0' AND
074900090211     C                             inpo121sse = 'D' AND
075000090211     C                             ds3idp.§3iPkd > 0 AND
075100090211     C                             (NInpo121Pkg = 0 OR
075200090211     C                              NInpo121Pkg > ds3idp.§3iPkd)
075300090211     c                   IF        $e < 10                                      *non + di 10 errori
075400090211     c                   ADD       1             $e
075500090211     c                   MOVEL     '9'           Serr($e)                       *aggiunge errore
075600090211     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0694'
075700090211     C                             : inpO121Lang : %EDITC(ds3idp.§3iPkd:'3'))
075800090211     c                   MOVEL     '1'           $err                           *controllore errore
075900090211     c                   ENDIF
076000090211     C                   ENDIF
076100090211     c*
076200020321     c                   ENDSR
076300020321     c*--------------------------------------------------------------------------------------------*
076400020321     c* controlla il numero colli
076500020321     c*--------------------------------------------------------------------------------------------*
076600020321     c     ChkInpNcl     BEGSR
076700020321     c*
076800020321     c* reimposta le variabili di lavoro
076900020321     c                   MOVEL     '0'           werr                           *NO errore
077000020321     c                   CLEAR                   NInpo121Ncl       5 0
077100020321     c*
077200020321     c* controlla il campo solo se immesso
077300020325if  1c                   IF        Inpo121Ncl <> *blanks
077400020325do  2c                   DO
077500020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
077600020321     c                   SETOFF                                       98
077700020321     c                   TESTN                   Inpo121Ncl           98
077800020325if  3c                   IF        *in98                                        *da TESTN è numero
077900020321     c                   MOVE      Inpo121Ncl    a1                             *ultimo carattere
078000020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
078100020321     c                   MOVEL     'S'           $OkNum                         *valido
078200020325e   4c                   ENDIF
078300020325e   3c                   ENDIF
078400020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
078500020321     c                   MOVEL     '1'           werr                           *errore Output
078600020321     c                   LEAVE
078700020325e   3c                   ENDIF
078800020321     c*
078900020321     c* sposta il campo in quello "normalizzato"
079000020321     c                   MOVEL     Inpo121Ncl    NInpo121Ncl                    *campo"normalizzato"
079100020321     c*
079200020325e   2c                   ENDDO
079300020325e   1c                   ENDIF
079400020321     c*
079500020321     c* se errore, memorizza il codice errore nella schiera errori
079600020325if  1c                   IF        werr = '1'
079700020325if  2c                   IF        $e < 10                                      *non + di 10 errori
079800020321     c                   ADD       1             $e
079900020321     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
080000060606     c*                  MOVEL     msg(10)       Smsg($e)                       *messaggio
080100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0410'
080200060609     C                             :inpO121lang)
080300060609     c                   MOVEL     '1'           $err                           *controllore errore
080400020325e   2c                   ENDIF
080500020325e   1c                   ENDIF
080600090211     ** Se richiesto il ritiro con DPD
080700090211     ** i colli diventano obbligatori e devono essere 1.
080800090211     C                   IF        wErr = '0' AND
080900090211     C                             inpo121sse = 'D' AND
081000090211     C                             NInpo121Ncl <> 1
081100090211     c                   IF        $e < 10                                      *non + di 10 errori
081200090211     c                   ADD       1             $e
081300090211     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
081400090211     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0693'
081500090211     C                             : inpO121Lang)
081600090211     c                   MOVEL     '1'           $err                           *controllore errore
081700090211     c                   ENDIF
081800090211     C                   ENDIF
081900090211     c*
082000020321     c                   ENDSR
082100000000     c*--------------------------------------------------------------------------------------------*
082200000000     c* controlla la data pronta merce
082300000000     c*--------------------------------------------------------------------------------------------*
082400000000     c     ChkInpDar     BEGSR
082500000000     c*
082600000000     c* reimposta le variabili di lavoro
082700000000     c                   MOVEL     '0'           werr                           *NO errore
082800000000     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
082900000000     c                   CLEAR                   wmsg                           *msg impostato
083000020325     c                   CLEAR                   tmsg                           *tipo msg impostato
083100020321     c                   CLEAR                   NInpo121Dar                    *campo"normalizzato"
083200000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
083300000000     c                   CLEAR                   dardep                         *depositi
083400000000     c*
083500000000     c* controlla se la data pronta merce è tutta un numero
083600000000do  1c                   DO
083700000000     c                   SETOFF                                       98
083800020321     c                   TESTN                   Inpo121Dar           98
083900000000if  2c                   IF        *in98                                        *da TESTN è numero
084000020321     c                   MOVE      Inpo121Dar    a1                             *ultimo carattere
084100000000if  3c                   IF        a1 >= '0'                                    *NON trovata lettera
084200000000     c                   MOVEL     'S'           $OkNum                         *valido
084300000000e   3c                   ENDIF
084400000000e   2c                   ENDIF
084500000000if  2c                   IF        $OkNum = 'N'                                 *NON valido
084600000000     c                   MOVEL     '1'           werr                           *errore Output
084700000000     c                   LEAVE
084800000000e   2c                   ENDIF
084900000000     c*
085000000000     c* sposta il campo in quello "normalizzato"
085100020321     c                   MOVEL     Inpo121Dar    NInpo121Dar                    *campo"normalizzato"
085200000000     c*
085300000000     c* controlla validità
085400020321     C                   MOVEL     NInpo121Dar   n8                             *data (8) in g/m/a
085500000000     C                   Z-ADD     n8            g08dat
085600000000     C                   Z-ADD     *zeros        g08inv
085700000000     C                   MOVEL     '0'           g08err
085800000000     c                   CALL      'XSRDA8'
085900000000     c                   PARM                    wlbda8
086000000000     c                   Z-ADD     g08inv        dardep
086100000000if  2c                   IF        g08err = '1'                                 *data errata
086200000000     c                   MOVEL     '1'           werr                           *errore Output
086300000000     c                   LEAVE
086400000000e   2c                   ENDIF
086500000000     c*
086600000000     c* reperimento data + ora corrente
086700000000     C                   TIME                    n14                            *reperimento ora
086800000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
086900000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
087000000000     C                   Z-ADD     n8            g08dat
087100000000     C                   Z-ADD     *zeros        g08inv
087200000000     C                   MOVEL     '0'           g08err
087300000000     c                   CALL      'XSRDA8'
087400000000     c                   PARM                    wlbda8
087500000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
087600000000     c*
087700000000     c* la data non può essere di un giorno PRIMA di oggi
087800000000if  2c                   IF        dardep < datcor
087900000000     c                   MOVEL     '1'           werr                           *errore Output
088000000000     c                   LEAVE
088100000000e   2c                   ENDIF
088200000000     c*
088300000000     c* se il tempo di ritiro del CAP di ritiro è -->
088400041124     c*  - "01" se l'ora corrente è entro le 12.30 allora la data ritiro può essere oggi
088500041124     c*         se l'ora corrente è dopo  le 12.30 allora la data ritiro NON può essere oggi
088600020321     c*  - "02" la data ritiro NON può essere oggi
088700000000if  2c                   IF        dardep = datcor
088800000000     C                   CLEAR                   tisi95ds
088900000000     C                   MOVEL     ' '           I95tla
089000000000     C                   MOVEL     '3'           I95tcn
089100020321     C                   MOVEL     InpO121Nar    I95nar
089200020321     C                   MOVEL     InpO121Car    I95cap
089300020321     C                   MOVEL     InpO121Lor    I95loc
089400020321     C                   MOVEL     InpO121Prr    I95prv
089500000000     C                   Z-ADD     datcor        I95dat
089600120829     C                   IF        i95nar = IRLANDA
089700120829     C                   EVAL      i95cap = EIRE
089800120829     C                   ENDIF
089900120829     C                   CALL      'TISI95R'                                    *reperimento tempo
090000000000     C                   PARM                    tisi95ds
090100000000     c*
090200000000if  3c                   IF        O95gf2 = '01' AND
090300041124     c                             oracor > 123000 OR
090400000000     c                             O95gf2 = '02'
090500000000     c                   MOVEL     '1'           werr                           *errore Output
090600000000     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
090700020325     c                   MOVEL     'C'           tmsg                           *tipo messaggio
090800060606     c*                  MOVEL     msg(12)       wmsg                           *messaggio
090900060609     C                   EVAL      wmsg = rtvMsgLang('TIS0301'
091000060609     C                             :inpO121lang)
091100060609     c                   LEAVE
091200000000e   3c                   ENDIF
091300000000e   2c                   ENDIF
091400000000     c*
091500000000e   1c                   ENDDO
091600000000     c*
091700000000     c* se errore, memorizza il codice errore nella schiera errori
091800000000if  1c                   IF        werr = '1'
091900000000if  2c                   IF        $e < 10                                      *non + di 10 errori
092000000000     c                   ADD       1             $e
092100000000if  3c                   IF        wimpmsg = '0'                                *msg da impostare
092200020325     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
092300060606     c*                  MOVEL     msg(11)       Smsg($e)                       *messaggio
092400060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0116'
092500060609     C                             :inpO121lang)
092600060609x   3c                   ELSE                                                   *msg già impostato
092700020325     c                   MOVEL     tmsg          Serr($e)                       *aggiunge errore
092800000000     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
092900000000e   3c                   ENDIF
093000000000     c                   MOVEL     '1'           $err                           *controllore errore
093100000000e   2c                   ENDIF
093200000000e   1c                   ENDIF
093300000000     c*
093400000000     c                   ENDSR
093500000000     c*--------------------------------------------------------------------------------------------*
093600000000     c* controlla l' ora pronta merce
093700000000     c*--------------------------------------------------------------------------------------------*
093800000000     c     ChkInpOrr     BEGSR
093900000000     c*
094000000000     c* reimposta le variabili di lavoro
094100000000     c                   MOVEL     '0'           werr                           *NO errore
094200020321     c                   CLEAR                   NInpo121Orr                    *campo"normalizzato"
094300000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
094400000000     c*
094500000000     c* controlla se l' ora pronta merce è tutta un numero
094600000000do  1c                   DO
094700000000     c                   SETOFF                                       98
094800020321     c                   TESTN                   Inpo121Orr           98
094900000000if  2c                   IF        *in98                                        *da TESTN è numero
095000020321     c                   MOVE      Inpo121Orr    a1                             *ultimo carattere
095100000000if  3c                   IF        a1 >= '0'                                    *NON trovata lettera
095200000000     c                   MOVEL     'S'           $OkNum                         *valido
095300000000e   3c                   ENDIF
095400000000e   2c                   ENDIF
095500000000if  2c                   IF        $OkNum = 'N'                                 *NON valido
095600000000     c                   MOVEL     '1'           werr                           *errore Output
095700000000     c                   LEAVE
095800000000e   2c                   ENDIF
095900000000     c*
096000000000     c* sposta il campo in quello "normalizzato"
096100020321     c                   MOVEL     Inpo121Orr    NInpo121Orr                    *campo"normalizzato"
096200000000     c*
096300000000     c* controlla validità
096400020321     C                   MOVEL     NInpo121Orr   n2                             *hh
096500000000if  2c                   IF        n2 >=  0 AND
096600000000     c                             n2 <= 23
096700000000x   2c                   ELSE
096800000000     c                   MOVEL     '1'           werr                           *errore Output
096900000000     c                   LEAVE
097000000000e   2c                   ENDIF
097100000000     c*
097200020321     C                   MOVE      NInpo121Orr   n2                             *mm
097300000000if  2c                   IF        n2 >=  0 AND
097400000000     c                             n2 <= 59
097500000000x   2c                   ELSE
097600000000     c                   MOVEL     '1'           werr                           *errore Output
097700000000     c                   LEAVE
097800000000e   2c                   ENDIF
097900000000     c*
098000000000e   1c                   ENDDO
098100000000     c*
098200000000     c* se errore, memorizza il codice errore nella schiera errori
098300000000if  1c                   IF        werr = '1'
098400000000if  2c                   IF        $e < 10                                      *non + di 10 errori
098500000000     c                   ADD       1             $e
098600020321     c                   MOVEL     'D'           Serr($e)                       *aggiunge errore
098700060606     c*                  MOVEL     msg(13)       Smsg($e)                       *messaggio
098800060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0423'
098900060609     C                             :inpO121lang)
099000060609     c                   MOVEL     '1'           $err                           *controllore errore
099100000000e   2c                   ENDIF
099200000000e   1c                   ENDIF
099300000000     c*
099400000000     c                   ENDSR
099500041126
099600041126     ***********************************************************************************************
099700041126     **
099800041126     ** Controllo data e ora pronta merce.
099900041126     **
100000041126     ***********************************************************************************************
100100041126     C     ChkDaROrR     BEGSR
100200041126
100300041126     C                   CALL(E)   'TIO708R'
100400041126     C                   PARM                    InpO121RQSCID
100500041126     C                   PARM                    InpO121KSU
100600041126     C                   PARM      *ZERO         CRO708           10
100700041126     C                   PARM                    InpO121CAR
100800041126     C                   PARM                    InpO121LOR
100900041126     C                   PARM                    InpO121PRR
101000041126     C                   PARM                    InpO121NAR
101100041126     C                   PARM                    InpO121DAR
101200041126     C                   PARM                    InpO121ORR
101300041126     C                   PARM      0             Esito
101400041126     C                   PARM      *BLANK        Msg708
101500060606     C                   PARM                    InpO121Lang
101600041126
101700041126     ** se errore, memorizza il codice errore nella schiera errori
101800041126     C                   IF        Esito < 0
101900041126if  2c                   IF        $e < 10                                      *non + di 10 errori
102000041126     c                   ADD       1             $e
102100041126     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
102200041126     c                   MOVEL     Msg708        Smsg($e)                       *messaggio
102300041126     c                   MOVEL     '1'           $err                           *controllore errore
102400041126e   2c                   ENDIF
102500041203     C                   ELSE
102600041203     c                   MOVEL     Inpo121Dar    NInpo121Dar                    *campo"normalizzato"
102700041203     c                   MOVEL     Inpo121Orr    NInpo121Orr                    *campo"normalizzato"
102800041126e   1c                   ENDIF
102900041126
103000041126     C                   ENDSR
103100041126
103200020328     c*--------------------------------------------------------------------------------------------*
103300020328     c* controlla la persona da contattare e il suo numero di telefono
103400020328     c*--------------------------------------------------------------------------------------------*
103500020328     c     ChkInpRer     BEGSR
103600020328     c*
103700020328     c* reimposta le variabili di lavoro
103800020328     c                   MOVEL     '0'           werr                           *NO errore
103900020328     c                   CLEAR                   NInpo121Rer                    *campi"normalizzati"
104000020328     c                   CLEAR                   NInpo121TeR
104100020328     c*
104200020328     c* campo obbligatorio
104300020328do  1c                   DO
104400020328if  2c                   IF        Inpo121Rer <> *blanks AND
104500020328     c                             Inpo121Ter <> *blanks
104600020328x   2c                   ELSE
104700020328     c                   MOVEL     '1'           werr                           *errore Output
104800020328     c                   LEAVE
104900020328e   2c                   ENDIF
105000020328     c*
105100020328     c* sposta il campo in quello "normalizzato"
105200020328     c     minu:maiu     XLATE     Inpo121Rer    NInpo121Rer                    *Minus -> Maiuscolo
105300020328     c     minu:maiu     XLATE     Inpo121Ter    NInpo121Ter                    *Minus -> Maiuscolo
105400020328e   1c                   ENDDO
105500020328     c*
105600020328     c* se errore, memorizza il codice errore nella schiera errori
105700020328if  1c                   IF        werr = '1'
105800020328if  2c                   IF        $e < 10                                      *non + di 10 errori
105900020328     c                   ADD       1             $e
106000020328     c                   MOVEL     'E'           Serr($e)                       *aggiunge errore
106100060606     c*                  MOVEL     msg(14)       Smsg($e)                       *messaggio
106200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0451'
106300060609     C                             :inpO121lang)
106400060609     c                   MOVEL     '1'           $err                           *controllore errore
106500020328e   2c                   ENDIF
106600020328e   1c                   ENDIF
106700020328     c*
106800020328     c                   ENDSR
106900020325     c*--------------------------------------------------------------------------------------------*
107000020325     c* controlla il pagamento
107100020325     c*--------------------------------------------------------------------------------------------*
107200020325     c     ChkInpPag     BEGSR
107300020325     c*
107400020325     c* reimposta le variabili di lavoro
107500020325     c                   MOVEL     '0'           werr                           *NO errore
107600020325     c*
107700020325     c* controlla validità scelta
107800020325do  1c                   DO
107900020325if  2c                   IF        Inpo121Pag = 'M' OR
108000020325     c                             Inpo121Pag = 'D' OR
108100020325     c                             Inpo121Pag = 'O'
108200020325x   2c                   ELSE
108300020325     c                   MOVEL     '1'           werr                           *SI errore
108400020325     c                   LEAVE
108500020325e   2c                   ENDIF
108600020325     c*
108700020325e   1C                   ENDDO
108800020325     c*
108900020325     c* se errore, memorizza il codice errore nella schiera errori
109000020325if  1c                   IF        werr = '1'
109100020325if  2c                   IF        $e < 10                                      *non + di 10 errori
109200020325     c                   ADD       1             $e
109300020325     c                   MOVEL     'F'           Serr($e)                       *aggiunge errore
109400060606     c*                  MOVEL     msg(15)       Smsg($e)                       *messaggio
109500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0140'
109600060609     C                             :inpO121lang)
109700060609     c                   MOVEL     '1'           $err                           *controllore errore
109800020325e   2c                   ENDIF
109900020325e   1c                   ENDIF
110000020325     c*
110100020325     c                   ENDSR
110200020321     c*--------------------------------------------------------------------------------------------*
110300020321     c* controlla la quantità merce
110400020321     c*--------------------------------------------------------------------------------------------*
110500020321     c     ChkInpQta     BEGSR
110600020321     c*
110700020321     c* reimposta le variabili di lavoro
110800020321     c                   MOVEL     '0'           werr                           *NO errore
110900020321     c                   CLEAR                   NInpo121Vlm       5 3
111000020321     c                   CLEAR                   NInpo121Bnc       5 0
111100020321     c                   CLEAR                   NInpo121Blc       5 0
111200020321     c                   CLEAR                   NInpo121Att       5 0
111300020321     c                   CLEAR                   NInpo121Mtc       5 0
111400020321     c                   CLEAR                   a5
111500020321     c*---
111600020321     c* Volume
111700020321     c*---
111800020321     c*
111900020321     c* controlla il campo solo se immesso
112000020321if  1c                   IF        werr = '0'
112100020321if  2c                   IF        Inpo121Vlm <> *blanks
112200020321     c     ' ' :'0'      XLATE     Inpo121Vlm    a5
112300020321do  3c                   DO
112400020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
112500020321     c                   SETOFF                                       98
112600020321     c                   TESTN                   a5                   98
112700020321if  4c                   IF        *in98                                        *da TESTN è numero
112800020321     c                   MOVE      a5            a1                             *ultimo carattere
112900020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
113000020321     c                   MOVEL     'S'           $OkNum                         *valido
113100020321e   5c                   ENDIF
113200020321e   4c                   ENDIF
113300020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
113400020321     c                   MOVEL     '1'           werr                           *errore Output
113500020321     c                   LEAVE
113600020321e   4c                   ENDIF
113700020321     c*
113800020321     c*
113900020321     c* sposta il campo in quello "normalizzato"
114000020321     c                   MOVEL     a5            NInpo121Vlm                    *campo"normalizzato"
114100020321e   3c                   ENDDO
114200020321e   2c                   ENDIF
114300020321     c*
114400020321     c* se errore, memorizza il codice errore nella schiera errori
114500020321if  2c                   IF        werr = '1'
114600020321if  3c                   IF        $e < 10                                      *non + di 10 errori
114700020321     c                   ADD       1             $e
114800020325     c                   MOVEL     'G'           Serr($e)                       *aggiunge errore
114900060606     c*                  MOVEL     msg(16)       Smsg($e)                       *messaggio
115000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0610'
115100060609     C                             :inpO121lang)
115200060609     c                   MOVEL     '1'           $err                           *controllore errore
115300020321e   3c                   ENDIF
115400020321e   2c                   ENDIF
115500020321e   1c                   ENDIF
115600020321     c*---
115700020321     c* Numero bancali
115800020321     c*---
115900020321     c*
116000020321     c* controlla il campo solo se immesso
116100020321if  1c                   IF        werr = '0'
116200020321if  2c                   IF        Inpo121Bnc <> *blanks
116300020321do  3c                   DO
116400020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
116500020321     c                   SETOFF                                       98
116600020321     c                   TESTN                   Inpo121Bnc           98
116700020321if  4c                   IF        *in98                                        *da TESTN è numero
116800020321     c                   MOVE      Inpo121Bnc    a1                             *ultimo carattere
116900020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
117000020321     c                   MOVEL     'S'           $OkNum                         *valido
117100020321e   5c                   ENDIF
117200020321e   4c                   ENDIF
117300020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
117400020321     c                   MOVEL     '1'           werr                           *errore Output
117500020321     c                   LEAVE
117600020321e   4c                   ENDIF
117700020321     c*
117800020321     c* sposta il campo in quello "normalizzato"
117900020321     c                   MOVEL     Inpo121Bnc    NInpo121Bnc                    *campo"normalizzato"
118000020321     c*
118100020321e   3c                   ENDDO
118200020321e   2c                   ENDIF
118300020321     c*
118400020321     c* se errore, memorizza il codice errore nella schiera errori
118500020321if  2c                   IF        werr = '1'
118600020321if  3c                   IF        $e < 10                                      *non + di 10 errori
118700020321     c                   ADD       1             $e
118800020325     c                   MOVEL     'H'           Serr($e)                       *aggiunge errore
118900060606     c*                  MOVEL     msg(17)       Smsg($e)                       *messaggio
119000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0408'
119100060609     C                             :inpO121lang)
119200060609     c                   MOVEL     '1'           $err                           *controllore errore
119300020321e   3c                   ENDIF
119400020321e   2c                   ENDIF
119500020321e   1c                   ENDIF
119600020321     c*---
119700020321     c* Peso + Volume + Bancali
119800020321     c*---
119900020321     c*
120000020321     c* occorre immettere almeno un valore fra questi
120100020321if  1c                   IF        werr = '0'
120200020321do  2c                   DO
120300020321if  3c                   IF        Inpo121Pkg > *zeros OR
120400020321     c                             Inpo121Vlm > *zeros  OR
120500020321     c                             Inpo121Bnc > *zeros
120600020321x   3c                   ELSE
120700020321     c                   MOVEL     '1'           werr                           *errore Output
120800020321     c                   LEAVE
120900020321e   3c                   ENDIF
121000020321     c*
121100020321e   2c                   ENDDO
121200020321     c*
121300020321     c* se errore, memorizza il codice errore nella schiera errori
121400020321if  2c                   IF        werr = '1'
121500020321if  3c                   IF        $e < 10                                      *non + di 10 errori
121600020321     c                   ADD       1             $e
121700020325     c                   MOVEL     'I'           Serr($e)                       *aggiunge errore
121800060606     c*                  MOVEL     msg(18)       Smsg($e)                       *messaggio
121900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0235'
122000060609     C                             :inpO121lang)
122100060609     c                   MOVEL     '1'           $err                           *controllore errore
122200020321e   3c                   ENDIF
122300020321e   2c                   ENDIF
122400020321e   1c                   ENDIF
122500020321     c*---
122600020321     c* Bilici
122700020321     c*---
122800020321     c*
122900020321     c* controlla il campo solo se immesso
123000020321if  1c                   IF        werr = '0'
123100020321if  2c                   IF        Inpo121Blc <> *blanks
123200020321do  3c                   DO
123300020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
123400020321     c                   SETOFF                                       98
123500020321     c                   TESTN                   Inpo121Blc           98
123600020321if  4c                   IF        *in98                                        *da TESTN è numero
123700020321     c                   MOVE      Inpo121Blc    a1                             *ultimo carattere
123800020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
123900020321     c                   MOVEL     'S'           $OkNum                         *valido
124000020321e   5c                   ENDIF
124100020321e   4c                   ENDIF
124200020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
124300020321     c                   MOVEL     '1'           werr                           *errore Output
124400020321     c                   LEAVE
124500020321e   4c                   ENDIF
124600020321     c*
124700020321     c* sposta il campo in quello "normalizzato"
124800020321     c                   MOVEL     Inpo121Blc    NInpo121Blc                    *campo"normalizzato"
124900020321     c*
125000020321e   3c                   ENDDO
125100020321e   2c                   ENDIF
125200020321     c*
125300020321     c* se errore, memorizza il codice errore nella schiera errori
125400020321if  2c                   IF        werr = '1'
125500020321if  3c                   IF        $e < 10                                      *non + di 10 errori
125600020321     c                   ADD       1             $e
125700020325     c                   MOVEL     'L'           Serr($e)                       *aggiunge errore
125800060606     c*                  MOVEL     msg(19)       Smsg($e)                       *messaggio
125900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0409'
126000060609     C                             :inpO121lang)
126100060609     c                   MOVEL     '1'           $err                           *controllore errore
126200020321e   3c                   ENDIF
126300020321e   2c                   ENDIF
126400020321e   1c                   ENDIF
126500020321     c*---
126600020321     c* Autotreni
126700020321     c*---
126800020321     c*
126900020321     c* controlla il campo solo se immesso
127000020321if  1c                   IF        werr = '0'
127100020321if  2c                   IF        Inpo121Att <> *blanks
127200020321do  3c                   DO
127300020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
127400020321     c                   SETOFF                                       98
127500020321     c                   TESTN                   Inpo121Att           98
127600020321if  4c                   IF        *in98                                        *da TESTN è numero
127700020321     c                   MOVE      Inpo121Att    a1                             *ultimo carattere
127800020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
127900020321     c                   MOVEL     'S'           $OkNum                         *valido
128000020321e   5c                   ENDIF
128100020321e   4c                   ENDIF
128200020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
128300020321     c                   MOVEL     '1'           werr                           *errore Output
128400020321     c                   LEAVE
128500020321e   4c                   ENDIF
128600020321     c*
128700020321     c* sposta il campo in quello "normalizzato"
128800020321     c                   MOVEL     Inpo121Att    NInpo121Att                    *campo"normalizzato"
128900020321     c*
129000020321e   3c                   ENDDO
129100020321e   2c                   ENDIF
129200020321     c*
129300020321     c* se errore, memorizza il codice errore nella schiera errori
129400020321if  2c                   IF        werr = '1'
129500020321if  3c                   IF        $e < 10                                      *non + di 10 errori
129600020321     c                   ADD       1             $e
129700020325     c                   MOVEL     'M'           Serr($e)                       *aggiunge errore
129800060606     c*                  MOVEL     msg(20)       Smsg($e)                       *messaggio
129900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0407'
130000060609     C                             :inpO121lang)
130100060609     c                   MOVEL     '1'           $err                           *controllore errore
130200020321e   3c                   ENDIF
130300020321e   2c                   ENDIF
130400020321e   1c                   ENDIF
130500020321     c*---
130600020321     c* Motrici
130700020321     c*---
130800020321     c*
130900020321     c* controlla il campo solo se immesso
131000020321if  1c                   IF        werr = '0'
131100020321if  2c                   IF        Inpo121Mtc <> *blanks
131200020321do  3c                   DO
131300020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
131400020321     c                   SETOFF                                       98
131500020321     c                   TESTN                   Inpo121Mtc           98
131600020321if  4c                   IF        *in98                                        *da TESTN è numero
131700020321     c                   MOVE      Inpo121Mtc    a1                             *ultimo carattere
131800020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
131900020321     c                   MOVEL     'S'           $OkNum                         *valido
132000020321e   5c                   ENDIF
132100020321e   4c                   ENDIF
132200020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
132300020321     c                   MOVEL     '1'           werr                           *errore Output
132400020321     c                   LEAVE
132500020321e   4c                   ENDIF
132600020321     c*
132700020321     c* sposta il campo in quello "normalizzato"
132800020321     c                   MOVEL     Inpo121Mtc    NInpo121Mtc                    *campo"normalizzato"
132900020321     c*
133000020321e   3c                   ENDDO
133100020321e   2c                   ENDIF
133200020321     c*
133300020321     c* se errore, memorizza il codice errore nella schiera errori
133400020321if  2c                   IF        werr = '1'
133500020321if  3c                   IF        $e < 10                                      *non + di 10 errori
133600020321     c                   ADD       1             $e
133700020325     c                   MOVEL     'N'           Serr($e)                       *aggiunge errore
133800060606     c*                  MOVEL     msg(21)       Smsg($e)                       *messaggio
133900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0411'
134000060609     C                             :inpO121lang)
134100060609     c                   MOVEL     '1'           $err                           *controllore errore
134200020321e   3c                   ENDIF
134300020321e   2c                   ENDIF
134400020321e   1c                   ENDIF
134500020321     c*
134600020321     c                   ENDSR
134700020325     c*--------------------------------------------------------------------------------------------*
134800020325     c* controlla gli orari di apertura UNO
134900020325     c*--------------------------------------------------------------------------------------------*
135000020325     c     ChkInpOA1     BEGSR
135100020325     c*
135200020325     c* reimposta le variabili di lavoro
135300020325     c                   MOVEL     '0'           werr                           *NO errore
135400020325     c                   CLEAR                   NInpo121AD1                    *campo"normalizzato"
135500020325     c                   CLEAR                   NInpo121AA1
135600020325     c*
135700020325     c* controlla i dati solo se immessi
135800020325if  1c                   IF        Inpo121AD1 <> *blanks OR
135900020325     c                             Inpo121AA1 <> *blanks
136000020325do  2c                   DO
136100020325     c*---
136200020325     c* Da
136300020325     c*---
136400020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
136500020325     c                   SETOFF                                       98
136600020325     c                   TESTN                   Inpo121AD1           98
136700020325if  3c                   IF        *in98                                        *da TESTN è numero
136800020325     c                   MOVE      Inpo121AD1    a1                             *ultimo carattere
136900020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
137000020325     c                   MOVEL     'S'           $OkNum                         *valido
137100020325e   4c                   ENDIF
137200020325e   3c                   ENDIF
137300020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
137400020325     c                   MOVEL     '1'           werr                           *errore Output
137500020325     c                   LEAVE
137600020325e   3c                   ENDIF
137700020325     c*
137800020325     c* sposta il campo in quello "normalizzato"
137900020325     c                   MOVEL     Inpo121AD1    NInpo121AD1                    *campo"normalizzato"
138000020325     c*
138100020325     c* controlla validità
138200020325     C                   MOVEL     NInpo121AD1   n2                             *hh
138300020325if  3c                   IF        n2 >=  0 AND
138400020325     c                             n2 <= 23
138500020325x   3c                   ELSE
138600020325     c                   MOVEL     '1'           werr                           *errore Output
138700020325     c                   LEAVE
138800020325e   3c                   ENDIF
138900020325     c*
139000020325     C                   MOVE      NInpo121AD1   n2                             *mm
139100020325if  3c                   IF        n2 >=  0 AND
139200020325     c                             n2 <= 59
139300020325x   3c                   ELSE
139400020325     c                   MOVEL     '1'           werr                           *errore Output
139500020325     c                   LEAVE
139600020325e   3c                   ENDIF
139700020325     c*---
139800020325     c* A
139900020325     c*---
140000020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
140100020325     c                   SETOFF                                       98
140200020325     c                   TESTN                   Inpo121AA1           98
140300020325if  3c                   IF        *in98                                        *da TESTN è numero
140400020325     c                   MOVE      Inpo121AA1    a1                             *ultimo carattere
140500020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
140600020325     c                   MOVEL     'S'           $OkNum                         *valido
140700020325e   4c                   ENDIF
140800020325e   3c                   ENDIF
140900020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
141000020325     c                   MOVEL     '1'           werr                           *errore Output
141100020325     c                   LEAVE
141200020325e   3c                   ENDIF
141300020325     c*
141400020325     c* sposta il campo in quello "normalizzato"
141500020325     c                   MOVEL     Inpo121AA1    NInpo121AA1                    *campo"normalizzato"
141600020325     c*
141700020325     c* controlla validità
141800020325     C                   MOVEL     NInpo121AA1   n2                             *hh
141900020325if  3c                   IF        n2 >=  0 AND
142000020325     c                             n2 <= 23
142100020325x   3c                   ELSE
142200020325     c                   MOVEL     '1'           werr                           *errore Output
142300020325     c                   LEAVE
142400020325e   3c                   ENDIF
142500020325     c*
142600020325     C                   MOVE      NInpo121AA1   n2                             *mm
142700020325if  3c                   IF        n2 >=  0 AND
142800020325     c                             n2 <= 59
142900020325x   3c                   ELSE
143000020325     c                   MOVEL     '1'           werr                           *errore Output
143100020325     c                   LEAVE
143200020325e   3c                   ENDIF
143300020325     c*---
143400020325     c* Da + A
143500020325     c*---
143600020325     c* l'ora A deve essere maggiore dell'ora DA
143700020325if  3c                   IF        NInpo121AA1 < NInpo121AD1
143800020325     c                   MOVEL     '1'           werr                           *errore Output
143900020325     c                   LEAVE
144000020325e   3c                   ENDIF
144100020325     c*
144200020325e   2c                   ENDDO
144300020325e   1c                   ENDIF
144400020325     c*
144500020325     c* se errore, memorizza il codice errore nella schiera errori
144600020325if  1c                   IF        werr = '1'
144700020325if  2c                   IF        $e < 10                                      *non + di 10 errori
144800020325     c                   ADD       1             $e
144900020325     c                   MOVEL     'O'           Serr($e)                       *aggiunge errore
145000060606     c*                  MOVEL     msg(22)       Smsg($e)                       *messaggio
145100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0457'
145200060609     C                             :inpO121lang)
145300060609     c                   MOVEL     '1'           $err                           *controllore errore
145400020325e   2c                   ENDIF
145500020325e   1c                   ENDIF
145600020325     c*
145700020325     c                   ENDSR
145800020325     c*--------------------------------------------------------------------------------------------*
145900020325     c* controlla gli orari di apertura DUE
146000020325     c*--------------------------------------------------------------------------------------------*
146100020325     c     ChkInpOA2     BEGSR
146200020325     c*
146300020325     c* reimposta le variabili di lavoro
146400020325     c                   MOVEL     '0'           werr                           *NO errore
146500020325     c                   CLEAR                   NInpo121AD2                    *campo"normalizzato"
146600020325     c                   CLEAR                   NInpo121AA2
146700020325     c*
146800020325     c* controlla i dati solo se immessi
146900020325if  1c                   IF        Inpo121AD2 <> *blanks OR
147000020325     c                             Inpo121AA2 <> *blanks
147100020325do  2c                   DO
147200020325     c*---
147300020325     c* Da
147400020325     c*---
147500020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
147600020325     c                   SETOFF                                       98
147700020325     c                   TESTN                   Inpo121AD2           98
147800020325if  3c                   IF        *in98                                        *da TESTN è numero
147900020325     c                   MOVE      Inpo121AD2    a1                             *ultimo carattere
148000020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
148100020325     c                   MOVEL     'S'           $OkNum                         *valido
148200020325e   4c                   ENDIF
148300020325e   3c                   ENDIF
148400020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
148500020325     c                   MOVEL     '1'           werr                           *errore Output
148600020325     c                   LEAVE
148700020325e   3c                   ENDIF
148800020325     c*
148900020325     c* sposta il campo in quello "normalizzato"
149000020325     c                   MOVEL     Inpo121AD2    NInpo121AD2                    *campo"normalizzato"
149100020325     c*
149200020325     c* controlla validità
149300020325     C                   MOVEL     NInpo121AD2   n2                             *hh
149400020325if  3c                   IF        n2 >=  0 AND
149500020325     c                             n2 <= 23
149600020325x   3c                   ELSE
149700020325     c                   MOVEL     '1'           werr                           *errore Output
149800020325     c                   LEAVE
149900020325e   3c                   ENDIF
150000020325     c*
150100020325     C                   MOVE      NInpo121AD2   n2                             *mm
150200020325if  3c                   IF        n2 >=  0 AND
150300020325     c                             n2 <= 59
150400020325x   3c                   ELSE
150500020325     c                   MOVEL     '1'           werr                           *errore Output
150600020325     c                   LEAVE
150700020325e   3c                   ENDIF
150800020325     c*---
150900020325     c* A
151000020325     c*---
151100020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
151200020325     c                   SETOFF                                       98
151300020325     c                   TESTN                   Inpo121AA2           98
151400020325if  3c                   IF        *in98                                        *da TESTN è numero
151500020325     c                   MOVE      Inpo121AA2    a1                             *ultimo carattere
151600020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
151700020325     c                   MOVEL     'S'           $OkNum                         *valido
151800020325e   4c                   ENDIF
151900020325e   3c                   ENDIF
152000020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
152100020325     c                   MOVEL     '1'           werr                           *errore Output
152200020325     c                   LEAVE
152300020325e   3c                   ENDIF
152400020325     c*
152500020325     c* sposta il campo in quello "normalizzato"
152600020325     c                   MOVEL     Inpo121AA2    NInpo121AA2                    *campo"normalizzato"
152700020325     c*
152800020325     c* controlla validità
152900020325     C                   MOVEL     NInpo121AA2   n2                             *hh
153000020325if  3c                   IF        n2 >=  0 AND
153100020325     c                             n2 <= 23
153200020325x   3c                   ELSE
153300020325     c                   MOVEL     '1'           werr                           *errore Output
153400020325     c                   LEAVE
153500020325e   3c                   ENDIF
153600020325     c*
153700020325     C                   MOVE      NInpo121AA2   n2                             *mm
153800020325if  3c                   IF        n2 >=  0 AND
153900020325     c                             n2 <= 59
154000020325x   3c                   ELSE
154100020325     c                   MOVEL     '1'           werr                           *errore Output
154200020325     c                   LEAVE
154300020325e   3c                   ENDIF
154400020325     c*---
154500020325     c* Da + A
154600020325     c*---
154700020325     c* l'ora A deve essere maggiore dell'ora DA
154800020325if  3c                   IF        NInpo121AA2 < NInpo121AD2
154900020325     c                   MOVEL     '1'           werr                           *errore Output
155000020325     c                   LEAVE
155100020325e   3c                   ENDIF
155200020325     c*
155300020325e   2c                   ENDDO
155400020325e   1c                   ENDIF
155500020325     c*
155600020325     c* se errore, memorizza il codice errore nella schiera errori
155700020325if  1c                   IF        werr = '1'
155800020325if  2c                   IF        $e < 10                                      *non + di 10 errori
155900020325     c                   ADD       1             $e
156000020325     c                   MOVEL     'P'           Serr($e)                       *aggiunge errore
156100060606     c*                  MOVEL     msg(23)       Smsg($e)                       *messaggio
156200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0541'
156300060609     C                             :inpO121lang)
156400060609     c                   MOVEL     '1'           $err                           *controllore errore
156500020325e   2c                   ENDIF
156600020325e   1c                   ENDIF
156700020325     c*
156800020325     c                   ENDSR
156900020325     c*--------------------------------------------------------------------------------------------*
157000020325     c* controlla gli orari di apertura UNO e DUE
157100020325     c*--------------------------------------------------------------------------------------------*
157200020325     c     ChkInpOA1OA2  BEGSR
157300020325     c*
157400020325     c* reimposta le variabili di lavoro
157500020325     c                   MOVEL     '0'           werr                           *NO errore
157600020325     c*
157700020325     c* se immessi primi e secondi occorre controllarne la congruità
157800020325if  1c                   IF        Inpo121AD1 <> *blanks AND
157900020325     c                             Inpo121AD2 <> *blanks
158000020325do  2c                   DO
158100020325     c*
158200020325if  3c                   IF        NInpo121AD2 < NInpo121AA1
158300020325     c                   MOVEL     '1'           werr                           *errore Output
158400020325     c                   LEAVE
158500020325e   3c                   ENDIF
158600020325     c*
158700020325e   2c                   ENDDO
158800020325e   1c                   ENDIF
158900020325     c*
159000020325     c* se errore, memorizza il codice errore nella schiera errori
159100020325if  1c                   IF        werr = '1'
159200020325if  2c                   IF        $e < 10                                      *non + di 10 errori
159300020325     c                   ADD       1             $e
159400020325     c                   MOVEL     'Q'           Serr($e)                       *aggiunge errore
159500060606     c*                  MOVEL     msg(24)       Smsg($e)                       *messaggio
159600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0171'
159700060609     C                             :inpO121lang)
159800060609     c                   MOVEL     '1'           $err                           *controllore errore
159900020325e   2c                   ENDIF
160000020325e   1c                   ENDIF
160100020325     c*
160200020325     c                   ENDSR
160300000000     c*--------------------------------------------------------------------------------------------*
160400000000     c* controlla consegna a uno o più destinatari
160500000000     c*--------------------------------------------------------------------------------------------*
160600000000     c     ChkInpUpd     BEGSR
160700000000     c*
160800000000     c* reimposta le variabili di lavoro
160900000000     c                   MOVEL     '0'           werr                           *NO errore
161000020325     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
161100000000     c                   CLEAR                   wmsg                           *msg impostato
161200020325     c                   CLEAR                   tmsg                           *tipo msg impostato
161300000000     c*
161400000000do  1c                   DO
161500000000     c*
161600000000     c* controlla validità campo
161700020321if  2c                   IF        Inpo121Upd = '1' OR
161800020321     c                             Inpo121Upd = '2'
161900000000x   2c                   ELSE
162000000000     c                   MOVEL     '1'           werr                           *errore Output
162100000000     c                   LEAVE
162200000000e   2c                   ENDIF
162300020325     c*
162400020326     c* se la consegna è a più di un destinatario, NON immettere il codice destinatario
162500020325if  2c                   IF        InpO121Upd <> '1' AND
162600020325     c                             NInpO121Cde > *zeros
162700020325     c                   MOVEL     '1'           werr                           *errore Output
162800020325     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
162900020325     c                   MOVEL     'S'           tmsg                           *tipo messaggio
163000060606     c*                  MOVEL     msg(26)       wmsg                           *messaggio
163100060609     C                   EVAL      wmsg = rtvMsgLang('TIS0291'
163200060609     C                             :inpO121lang)
163300060609     c                   LEAVE
163400020325e   2c                   ENDIF
163500101201     c*
163600101201     c* Se DPD, DEVE essere UN destinatario
163700101201if  2c                   IF        InpO121sse = 'D' AND
163800101201     c                             InpO121Upd <> '1'
163900101201     c                   MOVEL     '1'           werr                           *errore Output
164000101201     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
164100101201     c                   MOVEL     'S'           tmsg                           *tipo messaggio
164200101201     C                   EVAL      wmsg = rtvMsgLang('TIS0307'
164300101201     C                             :inpO121Lang)
164400101201     c                   LEAVE
164500101201e   2c                   ENDIF
164600101201     c*
164700000000e   1c                   ENDDO
164800000000     c*
164900000000     c* se errore, memorizza il codice errore nella schiera errori
165000000000if  1c                   IF        werr = '1'
165100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
165200000000     c                   ADD       1             $e
165300020325if  3c                   IF        wimpmsg = '0'                                *msg da impostare
165400020325     c                   MOVEL     'R'           Serr($e)                       *aggiunge errore
165500060606     c*                  MOVEL     msg(25)       Smsg($e)                       *messaggio
165600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0295'
165700060609     C                             :inpO121lang)
165800060609x   3c                   ELSE
165900020325     c                   MOVEL     tmsg          Serr($e)                       *aggiunge errore
166000020325     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
166100020325e   2c                   ENDIF
166200000000     c                   MOVEL     '1'           $err                           *controllore errore
166300000000e   2c                   ENDIF
166400000000e   1c                   ENDIF
166500000000     c*
166600000000     c                   ENDSR
166700000000     c*--------------------------------------------------------------------------------------------*
166800000000     c* controlla il destinatario
166900000000     c*--------------------------------------------------------------------------------------------*
167000000000     c     ChkInpDes     BEGSR
167100000000     c*
167200000000     c* reimposta le variabili di lavoro
167300000000     c                   MOVEL     '0'           werr                           *NO errore
167400020325     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
167500020325     c                   CLEAR                   wmsg                           *msg impostato
167600020325     c                   CLEAR                   tmsg                           *tipo msg impostato
167700020325     c*
167800020325     c* se la consegna è singola il destinatario è da inserire (codice o nome)
167900020325do  1c                   DO
168000020325if  2c                   IF        Inpo121Upd = '1'                             *singola
168100020325if  3c                   IF        (Inpo121Cde = *blanks OR
168200020325     c                             Inpo121Cde = *zeros) AND
168300020325     c                             (Inpo121Rsc = *blanks OR
168400020325     c                             Inpo121Inc = *blanks  OR
168500020325     c                             Inpo121Loc = *blanks  OR
168600020325     c                             Inpo121Cac = *blanks)
168700020325     c                   MOVEL     '1'           werr                           *errore Output
168800020325     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
168900020325     c                   MOVEL     'T'           tmsg                           *tipo messaggio
169000060606     c*                  MOVEL     msg(27)       wmsg                           *messaggio
169100060609     C                   EVAL      wmsg = rtvMsgLang('TIS0289'
169200060609     C                             :inpO121lang)
169300060609e   3c                   ENDIF
169400020325e   2c                   ENDIF
169500020325     c*
169600020326     c* se la consegna è a più di un destinatario, NON immettere il nome destinatario
169700020326     c* (la NON immissione del codice destinatario è già stato controllata sopra)
169800020325if  2c                   IF        InpO121Upd <> '1'
169900020325if  3c                   IF        InpO121Rsc > *blanks OR
170000020325     c                             Inpo121Inc > *blanks OR
170100020325     c                             Inpo121Loc > *blanks OR
170200020325     c                             Inpo121Cac > *blanks
170300020325     c                   MOVEL     '1'           werr                           *errore Output
170400020325     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
170500020325     c                   MOVEL     'U'           tmsg                           *tipo messaggio
170600060606     c*                  MOVEL     msg(28)       wmsg                           *messaggio
170700060609     C                   EVAL      wmsg = rtvMsgLang('TIS0292'
170800060609     C                             :inpO121lang)
170900060609     c                   LEAVE
171000020325e   3c                   ENDIF
171100020325e   2c                   ENDIF
171200020328     c*
171300020328     c* se è già stato inserito il codice destinatario, il nominativo per esteso è da lasciare vuoto
171400020328if  2c                   IF         InpO121Cde  > *zeros  AND
171500020328     c                             (Inpo121Rsc <> *blanks OR
171600020328     c                              Inpo121Inc <> *blanks OR
171700020328     c                              Inpo121Loc <> *blanks OR
171800020328     c                              Inpo121Cac <> *blanks)
171900020328     c                   MOVEL     '1'           werr                           *errore Output
172000020328     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
172100020328     c                   MOVEL     '£'           tmsg                           *tipo messaggio
172200060606     c*                  MOVEL     msg(38)       wmsg                           *messaggio
172300060609     C                   EVAL      wmsg = rtvMsgLang('TIS0294'
172400060609     C                             :inpO121lang)
172500060609e   2c                   ENDIF
172600000000     C*
172700000000e   1C                   ENDDO
172800000000     c*
172900000000     c* se errore, memorizza il codice errore nella schiera errori
173000000000if  1c                   IF        werr = '1'
173100000000if  2c                   IF        $e < 10                                      *non + di 10 errori
173200000000     c                   ADD       1             $e
173300020325     c                   MOVEL     tmsg          Serr($e)                       *aggiunge errore
173400020325     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
173500000000     c                   MOVEL     '1'           $err                           *controllore errore
173600000000e   2c                   ENDIF
173700000000e   1c                   ENDIF
173800020329     c*---
173900020329     c* Dati del destinatario
174000020329     c*---
174100020329if  1c                   IF        Inpo121Upd = '1' AND                         *singola e
174200020329     c                             InpO121Cde <= *zeros                         *codice NON inserito
174300000000     c*---
174400000000     c* Nominativo
174500000000     c*---
174600000000if  2c                   IF        werr <> '1'                                  *NO errori
174700000000     c                   EXSR      ChkInpRsc
174800000000e   2c                   ENDIF
174900000000     c*---
175000000000     c* Indirizzo
175100000000     c*---
175200000000if  2c                   IF        werr <> '1'                                  *NO errori
175300000000     c                   EXSR      ChkInpInc
175400000000e   2c                   ENDIF
175500000000     c*---
175600000000     c* Nazione
175700000000     c*---
175800000000if  2c                   IF        werr <> '1'                                  *NO errori
175900000000     c                   EXSR      ChkInpNac
176000000000e   2c                   ENDIF
176100020325     c*---
176200020325     c* Località
176300020325     c*---
176400020325if  2c                   IF        werr <> '1'                                  *NO errori
176500020325     c                   EXSR      ChkInpLoc
176600020325e   2c                   ENDIF
176700000000     c*---
176800000000     c* CAP
176900000000     c*---
177000000000if  2c                   IF        werr <> '1'                                  *NO errori
177100000000     c                   EXSR      ChkInpCac
177200000000e   2c                   ENDIF
177300000000     c*---
177400000000     c* provincia
177500000000     c*---
177600000000if  2c                   IF        werr <> '1'                                  *NO errori
177700000000     c                   EXSR      ChkInpPrc
177800000000e   2c                   ENDIF
177900000000     c*---
178000000000     c* Indirizzo + il CAP + la località + la provincia + la nazione
178100000000     c*---
178200000000if  2c                   IF        werr <> '1'                                  *NO errori
178300000000     c                   EXSR      ChkInpICKPNc
178400000000e   2c                   ENDIF
178500000000e   1c                   ENDIF
178600000000     c*
178700000000     c                   ENDSR
178800000000     c*--------------------------------------------------------------------------------------------*
178900000000     c* controlla il nominativo destinatario
179000000000     c*--------------------------------------------------------------------------------------------*
179100000000     c     ChkInpRsc     BEGSR
179200000000     c*
179300000000     c* reimposta le variabili di lavoro
179400000000     c                   MOVEL     '0'           werr                           *NO errore
179500020321     c                   CLEAR                   NInpo121Rsc                    *campo"normalizzato"
179600020326     c*
179700020326     c* controlla l'obbligatorietà del campo
179800020326do  1c                   DO
179900020326if  2c                   IF        Inpo121Rsc = *blanks
180000020326     c                   MOVEL     '1'           werr                           *SI errore
180100020326     c                   LEAVE
180200020326e   2c                   ENDIF
180300020326     C*
180400020326e   1c                   ENDDO
180500000000     c*
180600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
180700020321     c     minu:maiu     XLATE     Inpo121Rsc    NInpo121Rsc                    *Minus -> Maiuscolo
180800020326     c*
180900020326     c* se errore, memorizza il codice errore nella schiera errori
181000020326if  1c                   IF        werr = '1'
181100020326if  2c                   IF        $e < 10                                      *non + di 10 errori
181200020326     c                   ADD       1             $e
181300020326     c                   MOVEL     'V'           Serr($e)                       *aggiunge errore
181400060606     c*                  MOVEL     msg(29)       Smsg($e)                       *messaggio
181500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0480'
181600060609     C                             :inpO121lang)
181700060609     c                   MOVEL     '1'           $err                           *controllore errore
181800020326e   2c                   ENDIF
181900020326e   1c                   ENDIF
182000000000     c*
182100000000     c                   ENDSR
182200000000     c*--------------------------------------------------------------------------------------------*
182300000000     c* controlla l'indirizzo destinatario
182400000000     c*--------------------------------------------------------------------------------------------*
182500000000     c     ChkInpInc     BEGSR
182600000000     c*
182700000000     c* reimposta le variabili di lavoro
182800000000     c                   MOVEL     '0'           werr                           *NO errore
182900020321     c                   CLEAR                   NInpo121Inc                    *campo"normalizzato"
183000020326     c*
183100020326     c* controlla l'obbligatorietà del campo
183200020326do  1c                   DO
183300020326if  2c                   IF        Inpo121Inc = *blanks
183400020326     c                   MOVEL     '1'           werr                           *SI errore
183500020326     c                   LEAVE
183600020326e   2c                   ENDIF
183700020326     c*
183800020326e   1C                   ENDDO
183900000000     c*
184000000000     c* trasforma il campo immesso da minuscolo a maiuscolo
184100020321     c     minu:maiu     XLATE     Inpo121Inc    NInpo121Inc                    *Minus -> Maiuscolo
184200020326     c*
184300020326     c* se errore, memorizza il codice errore nella schiera errori
184400020326if  1c                   IF        werr = '1'
184500020326if  2c                   IF        $e < 10                                      *non + di 10 errori
184600020326     c                   ADD       1             $e
184700020326     c                   MOVEL     'Z'           Serr($e)                       *aggiunge errore
184800060606     c*                  MOVEL     msg(30)       Smsg($e)                       *messaggio
184900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0281'
185000060609     C                             :inpO121lang)
185100060609     c                   MOVEL     '1'           $err                           *controllore errore
185200020326e   2c                   ENDIF
185300020326e   1c                   ENDIF
185400000000     c*
185500000000     c                   ENDSR
185600000000     c*--------------------------------------------------------------------------------------------*
185700000000     c* controlla la nazione destinatario
185800000000     c*--------------------------------------------------------------------------------------------*
185900000000     c     ChkInpNac     BEGSR
186000000000     c*
186100000000     c* reimposta le variabili di lavoro
186200000000     c                   MOVEL     '0'           werr                           *NO errore
186300020321     c                   CLEAR                   NInpo121Nac                    *campo"normalizzato"
186400000000     c                   CLEAR                   deplad                         *deposito
186500000000     c*
186600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
186700000000do  1c                   DO
186800020321     c     minu:maiu     XLATE     Inpo121Nac    NInpo121Nac                    *Minus -> Maiuscolo
186900000000     c*
187000000000     c* controlla esistenza codice in anagrafica
187100020326if  2c                   IF        NInpo121Nac <> *blanks                       *se inserito
187200000000     c                   SETOFF                                           98
187300000000     c                   Z-ADD     1             i
187400020321     c     NInpo121Nac   LOOKUP    SNaz(i)                                98
187500000000if  3c                   IF        NOT *in98                                    *NON trovata
187600000000     c                   MOVEL     '1'           werr                           *SI errore
187700000000     c                   LEAVE
187800000000x   3c                   ELSE
187900000000     C                   Z-ADD     SNLad(i)      deplad                         *l.arrivo DPD
188000000000e   3c                   ENDIF
188100000000e   2c                   ENDIF
188200000000     C*
188300000000e   1c                   ENDDO
188400000000     c*
188500000000     c* se errore, memorizza il codice errore nella schiera errori
188600000000if  1c                   IF        werr = '1'
188700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
188800000000     c                   ADD       1             $e
188900020325     c                   MOVEL     'K'           Serr($e)                       *aggiunge errore
189000060606     c*                  MOVEL     msg(34)       Smsg($e)                       *messaggio
189100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0384'
189200060609     C                             :inpO121lang)
189300060609     c                   MOVEL     '1'           $err                           *controllore errore
189400000000e   2c                   ENDIF
189500000000e   1c                   ENDIF
189600000000     c*
189700000000     c                   ENDSR
189800000000     c*--------------------------------------------------------------------------------------------*
189900000000     c* controlla il CAP destinatario
190000000000     c*--------------------------------------------------------------------------------------------*
190100000000     c     ChkInpCac     BEGSR
190200000000     c*
190300000000     c* reimposta le variabili di lavoro
190400000000     c                   MOVEL     '0'           werr                           *NO errore
190500020321     c                   CLEAR                   NInpo121cac                    *campo"normalizzato"
190600020326     c*
190700020326     c* controlla l'obbligatorietà del campo
190800020326do  1c                   DO
190900020326if  2c                   IF        Inpo121cac = *blanks
191000020326     c                   MOVEL     '1'           werr                           *SI errore
191100020326     c                   LEAVE
191200020326e   2c                   ENDIF
191300020326     c*
191400020326e   1C                   ENDDO
191500000000     C*
191600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
191700020321     c     minu:maiu     XLATE     Inpo121Cac    NInpo121Cac                    *Minus -> Maiuscolo
191800020326     c*
191900020326     c* se errore, memorizza il codice errore nella schiera errori
192000020326if  1c                   IF        werr = '1'
192100020326if  2c                   IF        $e < 10                                      *non + di 10 errori
192200020326     c                   ADD       1             $e
192300020326     c                   MOVEL     'X'           Serr($e)                       *aggiunge errore
192400060606     c*                  MOVEL     msg(31)       Smsg($e)                       *messaggio
192500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0051'
192600060609     C                             :inpO121lang)
192700060609     c                   MOVEL     '1'           $err                           *controllore errore
192800020326e   2c                   ENDIF
192900020326e   1c                   ENDIF
193000000000     c*
193100000000     c                   ENDSR
193200000000     c*--------------------------------------------------------------------------------------------*
193300000000     c* controlla la località destinatario
193400000000     c*--------------------------------------------------------------------------------------------*
193500000000     c     ChkInpLoc     BEGSR
193600000000     c*
193700000000     c* reimposta le variabili di lavoro
193800000000     c                   MOVEL     '0'           werr                           *NO errore
193900020321     c                   CLEAR                   NInpo121Loc                    *campo"normalizzato"
194000020326     c*
194100020326     c* controlla l'obbligatorietà del campo
194200020326do  1c                   DO
194300020326if  2c                   IF        Inpo121Inc = *blanks
194400020326     c                   MOVEL     '1'           werr                           *SI errore
194500020326     c                   LEAVE
194600020326e   2c                   ENDIF
194700020326     c*
194800020326e   1C                   ENDDO
194900000000     C*
195000000000     c* trasforma il campo immesso da minuscolo a maiuscolo
195100020321     c     minu:maiu     XLATE     Inpo121Loc    NInpo121Loc                    *Minus -> Maiuscolo
195200020326     c*
195300020326     c* se errore, memorizza il codice errore nella schiera errori
195400020326if  1c                   IF        werr = '1'
195500020326if  2c                   IF        $e < 10                                      *non + di 10 errori
195600020326     c                   ADD       1             $e
195700020326     c                   MOVEL     'Y'           Serr($e)                       *aggiunge errore
195800060606     c*                  MOVEL     msg(32)       Smsg($e)                       *messaggio
195900060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0345'
196000060609     C                             :inpO121lang)
196100060609     c                   MOVEL     '1'           $err                           *controllore errore
196200020326e   2c                   ENDIF
196300020326e   1c                   ENDIF
196400000000     c*
196500000000     c                   ENDSR
196600000000     c*--------------------------------------------------------------------------------------------*
196700000000     c* controlla la provincia destinatario
196800000000     c*--------------------------------------------------------------------------------------------*
196900000000     c     ChkInpPrc     BEGSR
197000000000     c*
197100000000     c* reimposta le variabili di lavoro
197200000000     c                   MOVEL     '0'           werr                           *NO errore
197300020321     c                   CLEAR                   NInpo121Prc                    *campo"normalizzato"
197400000000     c*
197500000000     c* controlla l'obbligatorietà del campo
197600000000do  1c                   DO
197700000000     c*
197800000000     c* trasforma il campo immesso da minuscolo a maiuscolo
197900020321     c     minu:maiu     XLATE     Inpo121Prc    NInpo121Prc                    *Minus -> Maiuscolo
198000000000     c*
198100000000     c* controlla esistenza codice in anagrafica
198200020321if  2c                   IF        NInpo121Prc <> *blanks                       *se inserito
198300000000     c                   SETOFF                                           98
198400000000     c                   Z-ADD     1             i
198500020321     c     NInpo121Prc   LOOKUP    SPrv(i)                                98
198600000000if  3c                   IF        NOT *in98                                    *NON trovata
198700000000     c                   MOVEL     '1'           werr                           *SI errore
198800000000     c                   LEAVE
198900000000e   3c                   ENDIF
199000000000e   2c                   ENDIF
199100000000     c*
199200000000e   1C                   ENDDO
199300000000     c*
199400000000     c* se errore, memorizza il codice errore nella schiera errori
199500000000if  1c                   IF        werr = '1'
199600000000if  2c                   IF        $e < 10                                      *non + di 10 errori
199700000000     c                   ADD       1             $e
199800020325     c                   MOVEL     'J'           Serr($e)                       *aggiunge errore
199900060606     c*                  MOVEL     msg(33)       Smsg($e)                       *messaggio
200000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0461'
200100060609     C                             :inpO121lang)
200200060609     c                   MOVEL     '1'           $err                           *controllore errore
200300000000e   2c                   ENDIF
200400000000e   1c                   ENDIF
200500000000     c*
200600000000     c                   ENDSR
200700000000     c*--------------------------------------------------------------------------------------------*
200800000000     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione destinatario
200900000000     c*--------------------------------------------------------------------------------------------*
201000000000     c     ChkInpICKPNc  BEGSR
201100000000     c*
201200000000     c* reimposta le variabili di lavoro
201300000000     c                   MOVEL     '0'           werr                           *NO errore
201400000000     c*
201500020325     c* controlla esattezza dati destinatario, se inseriti
201600000000do  1c                   DO
201700000000     C                   CLEAR                   tisi95ds
201800000000     C                   MOVEL     ' '           I95tla
201900000000     C                   MOVEL     '7'           I95tcn
202000020321     C                   MOVEL     NInpo121Nac   I95nar
202100020321     C                   MOVEL     NInpo121Cac   I95cap
202200020321     C                   MOVEL     NInpo121Loc   I95loc
202300020321     C                   MOVEL     NInpo121Prc   I95prv
202400020321     C                   MOVEL     NInpo121Inc   I95ind
202500000000     C                   Z-ADD     datcor        I95dat
202600120829     C                   IF        i95nar = IRLANDA
202700120829     C                   EVAL      i95cap = EIRE
202800120829     C                   ENDIF
202900120829     C                   CALL      'TISI95R'
203000000000     C                   PARM                    tisi95ds
203100000000     c*
203200000000     c* se ritorna errore o dato non affidabile imposta errore
203300000000if  2c                   IF        O95Err <> *blanks
203400000000     c                   MOVEL     '1'           werr                           *SI errore
203500000000     c                   LEAVE
203600000000x   2c                   ELSE                                                   *NO errore
203700020321if  3c                   IF        NInpo121Nac = *blanks  AND                   *ITALIA
203800000000     c                             O95Lia <> '3'          OR                    *affidabilità 3
203900020321     c                             NInpo121Nac <> *blanks AND                   *ESTERO
204000000000     c                             O95Lia <> '2'                                *affidabilità 2
204100000000     c                   MOVEL     '1'           werr                           *SI errore
204200000000     c                   LEAVE
204300000000e   3c                   ENDIF
204400000000e   2c                   ENDIF
204500000000     c*
204600000000e   1c                   ENDDO
204700000000     c*
204800000000     c* se errore, memorizza il codice errore nella schiera errori
204900000000if  1c                   IF        werr = '1'
205000000000if  2c                   IF        $e < 10                                      *non + di 10 errori
205100000000     c                   ADD       1             $e
205200020325     c                   MOVEL     'W'           Serr($e)                       *aggiunge errore
205300060606     c*                  MOVEL     msg(35)       Smsg($e)                       *messaggio
205400060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0139'
205500060609     C                             :inpO121lang)
205600060609     c                   MOVEL     '1'           $err                           *controllore errore
205700000000e   2c                   ENDIF
205800000000e   1c                   ENDIF
205900000000     C*
206000000000     C                   ENDSR
206100000000     c*--------------------------------------------------------------------------------------------*
206200000000     c* controlla consegna a uno o più destinatari
206300000000     c*--------------------------------------------------------------------------------------------*
206400000000     c     ChkInpFfd     BEGSR
206500000000     c*
206600000000     c* reimposta le variabili di lavoro
206700000000     c                   MOVEL     '0'           werr                           *NO errore
206800000000     c*
206900000000     c* controlla validità campo
207000000000do  1c                   DO
207100020321if  2c                   IF        Inpo121Ffd = 'S' OR
207200020321     c                             Inpo121Ffd = 'N'
207300000000x   2c                   ELSE
207400000000     c                   MOVEL     '1'           werr                           *errore Output
207500000000     c                   LEAVE
207600000000e   2c                   ENDIF
207700000000     c*
207800000000e   1c                   ENDDO
207900000000     c*
208000000000     c* se errore, memorizza il codice errore nella schiera errori
208100000000if  1c                   IF        werr = '1'
208200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
208300000000     c                   ADD       1             $e
208400020325     c                   MOVEL     '!'           Serr($e)                       *aggiunge errore
208500060606     c*                  MOVEL     msg(36)       Smsg($e)                       *messaggio
208600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0537'
208700060609     C                             :inpO121lang)
208800060609     c                   MOVEL     '1'           $err                           *controllore errore
208900000000e   2c                   ENDIF
209000000000e   1c                   ENDIF
209100000000     c*
209200000000     c                   ENDSR
209300000000     c*--------------------------------------------------------------------------------------------*
209400000000     c* controlla la scelta del network estero
209500000000     c*--------------------------------------------------------------------------------------------*
209600000000     c     ChkInpSse     BEGSR
209700000000     c*
209800000000     c* reimposta le variabili di lavoro
209900000000     c                   MOVEL     '0'           werr                           *NO errore
210000000000     c*
210100000000     c* controlla validità campo
210200000000do  1c                   DO
210300020321if  2c                   IF        Inpo121Sse = 'N' OR
210400020321     c                             Inpo121Sse = 'E' OR
210500081119     c                             Inpo121Sse = 'D' OR
210600081119     c                             Inpo121Sse = *BLANK
210700081119x   2c                   ELSE
210800000000     c                   MOVEL     '1'           werr                           *errore Output
210900000000     c                   LEAVE
211000000000e   2c                   ENDIF
211100000000     c*
211200000000e   1c                   ENDDO
211300000000     c*
211400000000     c* se errore, memorizza il codice errore nella schiera errori
211500000000if  1c                   IF        werr = '1'
211600000000if  2c                   IF        $e < 10                                      *non + di 10 errori
211700000000     c                   ADD       1             $e
211800020325     c                   MOVEL     '"'           Serr($e)                       *aggiunge errore
211900060606     c*                  MOVEL     msg(37)       Smsg($e)                       *messaggio
212000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0536'
212100060609     C                             :inpO121lang)
212200060609     c                   MOVEL     '1'           $err                           *controllore errore
212300000000e   2c                   ENDIF
212400081119     c                   LEAVESR
212500081119e   1c                   ENDIF
212600081119     ** Verifica la ritirabilità all'estero.
212700081119     C                   IF        inpo121nar <> *BLANK AND
212800081119     C                             inpo121sse <> *BLANK AND inpo121sse <> 'N'
212900081119     C                   RESET                   fnlv12ds
213000081119     C                   RESET                   tisi95ds
213100081119     C                   RESET                   tisi97ds
213200081119     C                   EVAL      i12lang = inpo121lang
213300081119     C                   EVAL      i95nar = inpo121nar
213400081119     C                   EVAL      i95cap = inpo121car
213500081119     C                   EVAL      i95dat = %DEC(%DATE(inpo121dar : *EUR0))
213600081119     C                   IF        inpo121sse = 'D'
213700081119     C                   EVAL      i97ntw = 'DPD'
213800081119     C                   ELSEIF    inpo121sse = 'E'
213900081119     C                   EVAL      i97ntw = 'EEX'
214000081119     C                   ENDIF
214100081119     C                   CALL      'FNLV12R'
214200081119     C                   PARM                    fnlv12ds
214300081119     C                   PARM                    tisi95ds
214400081119     C                   PARM                    tisi97ds
214500081119     C                   IF        o12err <> *BLANK AND $e < 10
214600081119     C                   EVAL      $e += 1
214700081119     C                   EVAL      sErr($e) = 'R'
214800081119     C                   EVAL      sMsg($e) = o12msg
214900081119     C                   EVAL      $err = '1'
215000081119     C                   ENDIF
215100081119     C                   ENDIF
215200081119     c*
215300000000     c                   ENDSR
215400000000     c*--------------------------------------------------------------------------------------------*
215500000000     c* edita data da aaaammgg in gg/mm/aaaa
215600000000     c*--------------------------------------------------------------------------------------------*
215700000000     c     edtdat        BEGSR
215800000000     c*
215900000000     c                   MOVEL     *blanks       wdate
216000000000     c*
216100000000     c                   MOVEL     wdat          dsdat
216200000000     c                   MOVEL     dsgio         dsgioe
216300000000     c                   MOVEL     '/'           dsvd1
216400000000     c                   MOVEL     dsmes         dsmese
216500000000     c                   MOVEL     '/'           dsvd2
216600000000     c                   MOVEL     dsann         dsanne
216700000000     c                   MOVEL     dsdate        wdate
216800000000     c*
216900000000     c                   ENDSR
217000000000     c*--------------------------------------------------------------------------------------------*
217100000000     c* edita ora da hhmm in hh:mm
217200000000     c*--------------------------------------------------------------------------------------------*
217300000000     c     edtora        BEGSR
217400000000     c*
217500000000     c                   MOVEL     *blanks       worae
217600000000     c*
217700000000     c                   MOVEL     wora          dsora
217800000000     c                   MOVEL     dshh          dshhe
217900000000     c                   MOVEL     ':'           dsvo1
218000000000     c                   MOVEL     dsmm          dsmme
218100000000     c                   MOVEL     dsorae        worae
218200000000     c*
218300000000     c                   ENDSR
218400000000     c*--------------------------------------------------------------------------------------------*
218500000000     c* imposta i campi della DS di Output
218600000000     c*--------------------------------------------------------------------------------------------*
218700000000     c     ImpDsOut      BEGSR
218800000000     c*******************
218900000000     c* Note 1      -> Orari di apertura
219000000000     c*******************
219100020321if  1c                   IF        Inpo121AD1 <> *blanks OR
219200020321     c                             Inpo121AD2 <> *blanks
219300020321     c                   EVAL      OutO121No1 =  'Ap'
219400020321if  2c                   IF        Inpo121AD1 <> *blanks
219500020321     c                   EVAL      OutO121No1 =  %TRIM(OutO121No1) + ' ' +
219600020321     c                             NInpO121AD1 + ' ' + NInpO121AA1
219700000000e   2c                   ENDIF
219800020321if  2c                   IF        Inpo121AD2 <> *blanks
219900020321     c                   EVAL      OutO121No1 =  %TRIM(OutO121No1) + ' ' +
220000020321     c                             NInpO121AD2 + ' ' + NInpO121AA2
220100000000e   2c                   ENDIF
220200000000e   1c                   ENDIF
220300020325     c*******************
220400020325     c* PO emittente --> PO cliente PASSWORD
220500020325     c*******************
220600020325     c                   EVAL      OutO121Poe = %SUBST(InpO121Ksu:2:3)
220700020325     c*******************
220800020325     c* PO ritiro --> PO calcolato da CAP di ritiro, ma se scelto servizio DPD occorre prendere il
220900020325     c*               PO di arrivo DPD dalla nazione di ritiro
221000020325     c*******************
221100020325     c                   MOVEL     deplnaR       OutO121Por
221200020325if  1c                   IF        InpO121Sse = 'D' AND                         *servizio DPD
221300020325     c                             InpO121Nar <> *blanks                        *ritiro Estero
221400020325     c                   SETOFF                                           98
221500020325     c                   Z-ADD     1             i
221600020325     c     Inpo121Nar    LOOKUP    SNaz(i)                                98
221700020325if  2c                   IF        *in98                                        *NON trovata
221800020325     c                   MOVEL     SNLad(i)      OutO121Por
221900020325e   2c                   ENDIF
222000020325e   1c                   ENDIF
222100020325     c*******************
222200020325     c* PO consegna -> sempre uguale a zero
222300020325     c*******************
222400020325     c                   MOVEL     *zeros        OutO121Poc                     *NON calcolato
222500020326     c*******************
222600020326     c* Riferimento calcolato -> sempre uguale a zero
222700020326     c*******************
222800020326     c                   MOVEL     InpO121Rfa    OutO121Rfa
222900000000     c*
223000000000     c                   ENDSR
223100020327     c*--------------------------------------------------------------------------------------------*
223200020327     c* lancia la srittura dell'ORM
223300020327     c*--------------------------------------------------------------------------------------------*
223400020327     c     LanWriVpi     BEGSR
223500020327     c*
223600020327     c* azzera DS di passaggio
223700020327     c                   CLEAR                   tio110IDS
223800020327     c                   CLEAR                   tio110ODS
223900020327     c*
224000020327     c* imposta la DS di Input per lanciare il programma che scrive gli ORM
224100020327     c                   EVAL      InpO110RqscId =  Inpo121RqscId
224200020327     c                   EVAL      InpO110Ksu    =  Inpo121Ksu
224300050207     c                   EVAL      InpO110Cro    = *ZERO
224400020327     c                   EVAL      InpO110Rsr    = NInpO121Rsr
224500020327     c                   EVAL      InpO110Inr    = NInpO121Inr
224600020327     c                   EVAL      InpO110Nar    = NInpO121Nar
224700020327     c                   EVAL      InpO110Lor    = NInpO121Lor
224800020327     c                   EVAL      InpO110Car    = NInpO121Car
224900020327     c                   EVAL      InpO110Prr    = NInpO121Prr
225000020327     c                   EVAL      InpO110Pag    =  InpO121Pag
225100020327     c                   EVAL      InpO110Por    =  OutO121Por
225200020327     c                   EVAL      InpO110Poe    =  OutO121Poe
225300020327     c                   EVAL      InpO110Dar    = NInpo121Dar
225400020327     c                   EVAL      InpO110Orr    = NInpo121Orr
225500020327     c                   EVAL      InpO110AD1    = NInpo121AD1
225600020327     c                   EVAL      InpO110AA1    = NInpo121AA1
225700020327     c                   EVAL      InpO110AD2    = NInpo121AD2
225800020327     c                   EVAL      InpO110AA2    = NInpo121AA2
225900020327     c                   MOVEL     NInpo121Pkg   InpO110Pkg
226000020327     c                   MOVEL     NInpo121Vlm   InpO110Vlm
226100020327     c                   MOVEL     NInpo121Bnc   InpO110Bnc
226200020327     c                   MOVEL     NInpo121Ncl   InpO110Ncl
226300020327     c                   MOVEL     NInpo121Blc   InpO110Blc
226400020327     c                   MOVEL     NInpo121Att   InpO110Att
226500020327     c                   MOVEL     NInpo121Mtc   InpO110Mtc
226600020327     c                   EVAL      InpO110Nam    =  Inpo121Nam
226700020328     c                   EVAL      InpO110Rer    = NInpo121Rer
226800020328     c                   EVAL      InpO110Ter    = NInpo121Ter
226900050317     c                   EVAL      InpO110CttTlf =  Inpo121CttTlf
227000020327     c                   EVAL      InpO110Rfa    =  Inpo121Rfa
227100020327     c                   EVAL      InpO110Upd    =  Inpo121Upd
227200020327     c*---
227300020327     c* destinatario -->
227400020327     c* se     immesso il codice destinatario lo decodifica e riempe i campi del destinatario
227500020327     c* se NON immesso il codice destinatario i campi sono quelli immessi a video
227600020327     c*---
227700020329IF  1c                   IF        NInpO121Cde > *zeros
227800050207     c                   EVAL      Inpo110Crc    =  InpO121Cde
227900020327     c                   EVAL      Inpo110Rsc    =  deprsr
228000020327     c                   EVAL      Inpo110Inc    =  depinr
228100020327     c                   EVAL      Inpo110Nac    =  depnar
228200020327     c                   EVAL      Inpo110Loc    =  deplor
228300020327     c                   EVAL      Inpo110Cac    =  depcar
228400020327     c                   EVAL      Inpo110Prc    =  depprr
228500020327x   1c                   ELSE                                                   *NON immesso
228600050207     c                   EVAL      Inpo110Crc    =  *ZERO
228700020327     c                   EVAL      Inpo110Rsc    =  NInpo121Rsc
228800020327     c                   EVAL      Inpo110Inc    =  NInpo121Inc
228900020327     c                   EVAL      Inpo110Nac    =  NInpo121Nac
229000020327     c                   EVAL      Inpo110Loc    =  NInpo121Loc
229100020327     c                   EVAL      Inpo110Cac    =  NInpo121Cac
229200020327     c                   EVAL      Inpo110Prc    =  NInpo121Prc
229300020327e   1c                   ENDIF
229400020327     c                   EVAL      InpO110Ffd    =  Inpo121Ffd
229500020327     c                   EVAL      InpO110Sse    =  Inpo121Sse
229600020327     c                   EVAL      InpO110No2    =  Inpo121No2
229700020327     c                   EVAL      InpO110Poc    =  OutO121Poc
229800020327     c                   EVAL      InpO110Pr2    =  OutO121Por
229900020327     c                   EVAL      InpO110Pe2    =  OutO121Poe
230000020327     c                   EVAL      InpO110No1    =  OutO121No1
230100060606     c                   EVAL      InpO110Lang   =  InpO121Lang
230200040922     C                   CALL(E)   'TIO710R'
230300020327     c                   PARM                    tio110IDS
230400020327     c                   PARM                    tio110ODS
230500040922     c                   MOVEL     Outo110Err    SErr(1)                        *codice
230600040922     c                   MOVEL     Outo110Msg    SMsg(1)                        *messaggio
230700020328     c* SI errori
230800020329if  1C                   IF        %error OR OutO110ERR = '1'
230900020327     c                   MOVEL     '1'           $err                           *errore
231000020329x   1c                   ELSE
231100020329     c* NO errori
231200020329     c                   MOVEL     OutO110Rfa    OutO121Rfa                     *n.riferim.CALCOLATO
231300020327e   1c                   ENDIF
231400020327     C*
231500020327     c                   ENDSR
231600000000     c*--------------------------------------------------------------------------------------------*
231700000000     c* caricamento tabelle occorrenti
231800000000     c*--------------------------------------------------------------------------------------------*
231900000000     c     cartab        BEGSR
232000000000     c*---
232100000000     c* organigramma
232200000000     c*---
232300000000     c                   CLEAR                   i                              *indice
232400000000     C                   CLEAR                   sorg
232500000000     C                   CLEAR                   sodes
232600000000     C                   CLEAR                   sofl1
232700000000     C                   CLEAR                   sopt
232800000000     C                   CLEAR                   sodpd
232900000000     c     *loval        SETLL     azorg01l
233000000000     c                   READ      azorg01l                               99
233100000000do  1c                   DOW       NOT *in99
233200000000     c                   ADD       1             i
233300000000     c                   Z-ADD     orgfil        sorg(i)
233400000000     c                   MOVEL     orgdes        sodes(i)
233500000000     c                   MOVEL     orgfl1        sofl1(i)
233600000000     c                   MOVEL     orgde3        og143
233700000000     c                   MOVEL     §ogpt         sopt(i)
233800000000     c                   MOVEL     §ogdpd        sodpd(i)
233900000000     c                   READ      azorg01l                               99
234000000000e   1c                   ENDDO
234100000000     c*---
234200000000     c* provincie
234300000000     c*---
234400000000     c                   CLEAR                   i                              *indice
234500000000     C                   CLEAR                   dspr
234600000000     C                   CLEAR                   SPrv
234700000000     C                   CLEAR                   SPDes
234800020906     C                   Z-ADD     1             kblkut
234900000000     C                   MOVEL     'PR'          kblcod
235000000000     c     keytab        SETLL     tabel00f
235100000000     c     keytab        READE     tabel00f                               99
235200000000do  1c                   DOW       NOT *in99
235300000000if  2c                   IF        tblflg = *blanks
235400000000     c                   MOVEL     tbluni        dspr
235500000000     c                   ADD       1             i
235600000000     c                   MOVEL     tblkey        SPrv(i)
235700000000     c                   MOVEL     §Prdes        SPDes(i)
235800000000e   2c                   ENDIF
235900000000     c     keytab        READE     tabel00f                               99
236000000000e   1c                   ENDDO
236100000000     c*---
236200000000     c* nazioni
236300000000     c*---
236400000000     c                   CLEAR                   i                              *indice
236500000000     C                   CLEAR                   ds15
236600000000     C                   CLEAR                   SNaz
236700000000     C                   CLEAR                   SNDes
236800020906     C                   Z-ADD     1             kblkut
236900000000     C                   MOVEL     '15'          kblcod
237000000000     c     keytab        SETLL     tabel00f
237100000000     c     keytab        READE     tabel00f                               99
237200000000do  1c                   DOW       NOT *in99
237300000000if  2c                   IF        tblflg = *blanks
237400000000     c                   MOVEL     tbluni        ds15
237500000000     c                   ADD       1             i
237600000000     c                   MOVEL     tblkey        SNaz(i)
237700000000     c                   MOVEL     §15des        SNDes(i)
237800000000     c                   Z-ADD     §15lad        SNLad(i)
237900000000e   2c                   ENDIF
238000000000     c     keytab        READE     tabel00f                               99
238100000000e   1c                   ENDDO
238200090211     c*---
238300090211     c* Regole ritiro DPD.
238400090211     c*---
238500090211     c                   EVAL      kblkut = 1
238600090211     c                   EVAL      kblcod = '3I'
238700090211     c                   EVAL      kblkey = 'DPD'
238800090211     c     keyTab3       CHAIN     tabel00f
238900090211     c                   IF        %FOUND() AND tblFlg = *BLANK
239000090211     c                   EVAL      ds3idp = tblUni
239100090211     c                   ELSE
239200090211     c                   CLEAR                   ds3idp
239300090211     c                   ENDIF
239400090211     c*
239500000000     c                   ENDSR
239600000000     c*--------------------------------------------------------------------------------------------*
239700000000     c* operazioni finali
239800000000     c*--------------------------------------------------------------------------------------------*
239900000000     c     rutfin        BEGSR
240000000000     c*
240100000000     c* chiude pgm
240200000000     C                   CLEAR                   tisi95ds
240300000000     C                   MOVEL     'C'           I95tla
240400000000     C                   CALL      'TISI95R'                                    *controlla CAP
240500000000     C                   PARM                    tisi95ds
240600000000     c*
240700000000     c                   ENDSR
240800000000     c*--------------------------------------------------------------------------------------------*
240900000000     c* operazioni iniziali -da fare sempre-
241000000000     c*--------------------------------------------------------------------------------------------*
241100000000     c     rutinz        BEGSR
241200000000     c*
241300000000     c* reperimento data corrente
241400000000     C                   TIME                    n14                            *ora (6)+ data (8)
241500020321     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
241600000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
241700000000     C                   Z-ADD     n8            g08dat
241800000000     C                   Z-ADD     *zeros        g08inv
241900000000     C                   MOVEL     '0'           g08err
242000000000     c                   CALL      'XSRDA8'
242100000000     c                   PARM                    wlbda8
242200000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
242300000000     c*
242400000000     c* azzera la DS di Output e le variabili di lavro
242500020321     c                   CLEAR                   tio121ODs                      *Ds Output
242600020321     c                   MOVEL     *ALL'0'       Outo121Err                     *NO errore
242700000000     c                   MOVEL     *all'0'       SErr                           *schiera errori
242800000000     c                   MOVEL     *blanks       SMsg                           *schiera msg errori
242900000000     c                   Z-ADD     *zeros        $e                             *indice  errori
243000000000     c                   MOVEL     '0'           $err                           *controllore errore
243100000000     c*
243200000000     c* azzera le variabili "normalizzate"
243300020325     c                   CLEAR                   NInpo121Cde
243400020321     c                   CLEAR                   NInpo121Rsr
243500020321     c                   CLEAR                   NInpo121Inr
243600020321     c                   CLEAR                   NInpo121Nar
243700020321     c                   CLEAR                   NInpo121Lor
243800020321     c                   CLEAR                   NInpo121Car
243900020321     c                   CLEAR                   NInpo121Prr
244000020321     c                   CLEAR                   NInpo121Pkg
244100020321     c                   CLEAR                   NInpo121Ncl
244200020321     c                   CLEAR                   NInpo121Dar
244300020321     c                   CLEAR                   NInpo121Orr
244400020328     c                   CLEAR                   NInpo121Rer
244500020328     c                   CLEAR                   NInpo121Ter
244600020321     c                   CLEAR                   NInpo121Vlm
244700020321     c                   CLEAR                   NInpo121Bnc
244800020321     c                   CLEAR                   NInpo121Blc
244900020321     c                   CLEAR                   NInpo121Att
245000020321     c                   CLEAR                   NInpo121Mtc
245100020321     c                   CLEAR                   NInpo121AD1
245200020321     c                   CLEAR                   NInpo121AA1
245300020321     c                   CLEAR                   NInpo121AD2
245400020321     c                   CLEAR                   NInpo121AA2
245500020321     c                   CLEAR                   NInpo121Rsc
245600020321     c                   CLEAR                   NInpo121Inc
245700020321     c                   CLEAR                   NInpo121Nac
245800020321     c                   CLEAR                   NInpo121Loc
245900020321     c                   CLEAR                   NInpo121Cac
246000020321     c                   CLEAR                   NInpo121Prc
246100000000     c*
246200000000     c                   ENDSR
246300000000     c*--------------------------------------------------------------------------------------------*
246400000000     c* Operazioni iniziali -solo la prima volta-
246500000000     c*--------------------------------------------------------------------------------------------*
246600000000     C     *InzSr        BEGSR
246700000000     c*
246800000000     c* ricevimento parametri
246900000000     c     *ENTRY        PLIST
247000020321     c                   PARM                    tio121IDS
247100020321     c                   PARM                    tio121ODS
247200000000     c*
247300000000     c* chiavi di lettura
247400000000     c     keytab        KLIST                                                  tabel00f
247500000000     c                   KFLD                    kblkut                         -utente
247600000000     c                   KFLD                    kblcod                         -tabella
247700090211     c     keyTab3       KLIST                                                  tabel00f
247800090211     c                   KFLD                    kblkut                         -utente
247900090211     c                   KFLD                    kblcod                         -tabella
248000090211     c                   KFLD                    kblkey                         -elemento
248100090211     c     keytbe        KLIST                                                  *tntbe01l
248200000000     c                   KFLD                    kbecod                          -tabella
248300000000     c                   KFLD                    kbeke1                          -chiave uno
248400000000     c                   KFLD                    kbeke2                          -chiave due
248500000000     c                   KFLD                    kbelin                          -lingua
248600000000     c                   KFLD                    kbesif                          -s.informativo
248700000000     c     ke2tbe        KLIST                                                  *tntbe02l
248800000000     c                   KFLD                    kbecod                          -tabella
248900000000     c                   KFLD                    kbelin                          -lingua
249000000000     c                   KFLD                    kbesif                          -s.informativo
249100020321     c     keyvss        KLIST                                                  *tivss02l
249200020321     c                   KFLD                    kssksu                          -cl.unificante
249300020321     c                   KFLD                    kssisv                          -tipo servizio
249400020321     c     keyacr        KLIST                                                  *fnacr01l
249500020321     c                   KFLD                    kcrcro                          -cl.ritiro
249600000000     c*
249700020327     c* aggiunge le librerie in lista
249800041126     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
249900041126     c*                  CALL      wpgm
250000000000     c*
250100000000     c* apre i files
250200020321     c                   OPEN      tivss02l
250300020321     c                   OPEN      fnacr01l
250400000000     c                   OPEN      tabel00f
250500000000     c                   OPEN      tntbe01l
250600000000     c                   OPEN      tntbe02l
250700000000     c                   OPEN      azorg01l
250800000000     c*
250900000000     c* caricamento tabelle occorrenti
251000000000     c                   EXSR      cartab
251100000000     c*
251200000000     c                   ENDSR
251300020321     c*--------------------------------------------------------------------------------------------*
251400020321     C* Memorizza in schiere i clienti ritiro dell'utente STRATEGI
251500020321     c*--------------------------------------------------------------------------------------------*
251600020321     C     CarAcr        BEGSR
251700020321     C*
251800020321     C* azzera le variabili di lavoro
251900020321     c                   CLEAR                   i                              *indici
252000020321     c                   CLEAR                   ii
252100020321     c                   CLEAR                   SCro                           *codici clienti
252200020321     C*
252300020321     c* carica i clienti figli legati al cliente unificante di entrata
252400020321     c                   EXSR      CarKun
252500020321     C*
252600020321     C* per ciascun cliente figlio memorizzato (o solo il padre se non ci sono figli)
252700020321     c                   Z-ADD     1             i
252800020321     c                   Z-ADD     *zeros        ii
252900020321do  1C                   DOW       sksc7(i) > *zeros AND                        *piena schiera INP
253000020321     C                             i  <= 500         AND                        *fine  schiera INP
253100020321     C                             ii <  $dim                                   *fine  schiera OUT
253200020321     C*
253300020321     C* memorizza tutti i suoi clienti ritiro + progressivo
253400020321     c                   MOVEL     sksc7(i)      wacr
253500020321     c                   MOVE      000           wacr
253600020321     c                   Z-ADD     wacr          kcrcro
253700020321     c     keyacr        SETLL     fnacr01l
253800020321     c                   READ      fnacr01l                               98
253900020321     c*
254000020321     c* memorizza il cliente ritiro letto (senza prg) in una variabile di lavoro
254100020321     c                   MOVEL     acrcro        wcli                           *cliente senza prg
254200020321     c*
254300020321     c* ciclo fino a fine file o rottura cliente ritiro (senza prg)
254400020321do  2C                   DOW       ii <  $dim        AND                        *fine  schiera OUT
254500020321     c                             NOT *in98         AND                        *fine file
254600020321     c                             sksc7(i) = wcli                              *fine prg cliente
254700020321     c* solo record validi
254800020321if  3c                   IF        acratb = *blanks  AND                        *NO annullati
254900020321     c                             acrtcr <> 'M'                                *NO infrequenti
255000020321     c* memorizza i dati
255100020321     c                   EVAL      ii = ii + 1                                  *indice
255200020321     c                   EVAL      SCro(ii) = acrcro                            *codice cliente
255300020321e   3c                   ENDIF
255400020321     c*
255500020321     c* lettura successiva cliente ritiro
255600020321     c                   READ      fnacr01l                               98
255700020321     c*
255800020321     c* memorizza il cliente ritiro letto (senza prg) in una variabile di lavoro
255900020321     c                   MOVEL     acrcro        wcli                           *cliente senza prg
256000020321e   2c                   ENDDO                                                  *rottura cliente
256100020321     C*
256200020321     c                   ADD       1             i                              *lettura cl.figlio
256300020321e   1c                   ENDDO                                                  *fine clienti figli
256400020321     C*
256500020321     C                   ENDSR
256600020321     c*--------------------------------------------------------------------------------------------*
256700020321     c* Carica i clienti figli legati al cliente unificante di entrata
256800020321     c*--------------------------------------------------------------------------------------------*
256900020321     c     CarKun        BEGSR
257000020321     c*
257100020321     c* reimposta variabili di lavoro
257200020321     c                   CLEAR                   sksc7                          clienti da elaborare
257300020321     c*
257400020321     c* reperisce il legame clienti figli -se c'è- sul file delle abilitazioni
257500020321     c                   EVAL      kssksu = InpO121Ksu                          *cliente unificante
257600020321     c                   EVAL      kssisv = 'TT'                                *tipo servizio
257700020321     c     keyvss        SETLL     tivss02l
257800020321     c     keyvss        READE     tivss02l                               99
257900020321do  1c                   DOW       NOT *in99
258000020321if  2c                   IF        vssvat =  *blanks                            *attivo
258100020321if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
258200020321     c                             datcor <= vssdsc
258300020321     c*
258400020321     c* controlla, se per il cliente unificante, ci sono clienti figli da elaborare
258500020321if  4c                   IF        vsstle <> *blanks                            *SI legami
258600020321     c                   CLEAR                   tibs10ds
258700020321     c                   EVAL      d10drf = datcor                              *data riferimento
258800020321     c                   EVAL      d10tle = vsstle                              *legame
258900020321     c                   EVAL      d10paf = 'F'                                 *cerca i figli
259000020321     c                   MOVEL     vssksu        n8
259100020321     c                   Z-ADD     n8            d10cod                         *cliente unificante
259200020321     c                   CALL      'TIBS10R'                                    *cerca figli
259300020321     c                   PARM                    tibs10ds
259400020321if  5c                   IF        d10err <> ' '                                *SI errore: no figli
259500020321     c                   MOVE      InpO121Ksu    sksc7(1)                       *cliente unificante
259600020321x   5c                   ELSE                                                   *NO errore: si figli
259700020321     c                   Z-ADD     sksc11        sksc7                          *cliente unif+figli
259800020321e   5c                   ENDIF
259900020321x   4c                   ELSE                                                   *NO legami
260000020321     c                   MOVE      InpO121Ksu    sksc7(1)                       *cliente unificante
260100020321e   4c                   ENDIF
260200020321     c*
260300020321     c* se trovato cliente abilitato valido, esce dal ciclo di lettura
260400020321     c                   LEAVE                                                  *uscita ciclo
260500020321e   3c                   ENDIF
260600020321e   2c                   ENDIF
260700020321     c     keyvss        READE     tivss02l                               99
260800020321e   1c                   ENDDO                                                  *fine cl.abilitati
260900020321     c*
261000020321     c                   ENDSR
261100020321     c*--------------------------------------------------------------------------------------------*
261200060605
261300000000** -MSG- Messaggi di uscita                                                        num  cod
261400020325/ Codice cliente destinatario errato                                                1    1
261500020325/ Ragione sociale ritiro errata                                                     2    2
261600020325/ Indirizzo ritiro errato                                                           3    3
261700020325/ CAP ritiro errato                                                                 4    4
261800020325/ Località ritiro errata                                                            5    5
261900020325/ Provincia ritiro errata                                                           6    6
262000020325/ Nazione ritiro errata                                                             7    7
262100020321/ CAP, località e provincia ritiro incongruenti fra loro                            8    8
262200020321/ Peso errato                                                                       9    9
262300020321/ Numero colli errato                                                               10   A
262400020321/ Data di ritiro errata                                                             11   B
262500020321/ L' orario attuale non consente di effettuare il ritiro in giornata                12   C
262600020321/ Ora  pronta merce errata                                                          13   D
262700020328/ Persona e telefono per il ritiro errati                                           14   E
262800020325/ Dati pagamento errati                                                             15   F
262900020325/ Volume errato                                                                     16   G
263000020325/ Numero bancali errato                                                             17   H
263100020325/ Immettere almeno una quantità fra peso, volume o bancali                          18   I
263200020325/ Numero bilici  errato                                                             19   L
263300020325/ Numero autotreni errato                                                           20   M
263400020325/ Numero motrici errato                                                             21   N
263500020325/ Prima   ora apertura errata                                                       22   O
263600020325/ Seconda ora apertura errata                                                       23   P
263700020325/ Errori di congruità fra gli orari di apertura indicati                            24   Q
263800020325/ Inserire se la consegna è da effettuarsi da uno o più destinatari                 25   R
263900020325/ Inserire il codice destinatario se la consegna è per un SOLO destinatario         26   S
264000020325/ Inserire i dati del destinatario (codice o nome)                                  27   T
264100020325/ Inserire il nome   destinatario se la consegna è per un SOLO destinatario         28   U
264200020326/ Ragione sociale destinatario errata                                               29   V
264300020326/ Indirizzo destinatario errato                                                     30   Z
264400020326/ CAP destinatario errato                                                           31   X
264500020326/ Località destinatario errata                                                      32   Y
264600020325/ Provincia destinatario errata                                                     33   J
264700020325/ Nazione destinatario errata                                                       34   K
264800020325/ Dati destinatario errati                                                          35   W
264900020325/ Scegliere la modalità di consegna                                                 36   !
265000020325/ Scegliere il network estero                                                       37   "
265100020328/ Inserire o il codice o il nominativo destinatario per esteso                      38   £
265200020325                                                                                    39   $
265300020327                                                                                    40   %
