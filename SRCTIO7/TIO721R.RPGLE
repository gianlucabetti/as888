000100130423     H dftactgrp(*NO) actgrp(*CALLER) bnddir('TIS':'TIBS')
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
003000130423     f*azorg01l  IF   E           K DISK    USROPN
003100060605
003200130423      /COPY GAITRASRC/SRCPROTOPR,TIBSORG00
003300130423
003400060609     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
003500060612     D  msgId                         7A   CONST
003600060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
003700060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003800060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003900060612     D                                     VARYING
004000060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
004100060612     D                                     VARYING
004200060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
004300060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
004400060612
004500060605     d*--------------------------------------------------------------------------------------------*
004600000000     d* Data structure
004700000000     d*--------------------------------------------------------------------------------------------*
004800040921     D                SDS
004900040921     D  PgmLibrary            81     90
005000000000     d*---
005100000000     d* Costanti
005200000000     d*---
005300000000     d minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005400000000     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005500000000     d $Dim            C                   CONST(200)                           *n° clienti da memo
005600120829     D IRLANDA         C                   'IRL'
005700120829     D EIRE            C                   'EIRE'
005800120829     d*---
005900000000     d* Schiere a tempo di compilazione
006000000000     d*---
006100000000     d msg             S             80    DIM(40) CTDATA PERRCD(1)             *messaggi di uscita
006200000000     d*---
006300000000     d* Variabili riferite al data base
006400000000     d*---
006500000000     d kblkut          S                   LIKE(tblkut)                         *lettura tabel00f
006600000000     d kblcod          S                   LIKE(tblcod)
006700090211     d kblkey          S                   LIKE(tblkey)
006800090211     d kbecod          S                   LIKE(tbecod)                         *lettura tntbe01l
006900000000     d kbeke1          S                   LIKE(tbeke1)
007000000000     d kbeke2          S                   LIKE(tbeke2)
007100000000     d kbelin          S                   LIKE(tbelin)
007200000000     d kbesif          S                   LIKE(tbesif)
007300020321     d kcrcro          S                   LIKE(acrcro)                         *lettura fnacr00f
007400020321     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
007500020321     d kssisv          S                   LIKE(vssisv)
007600020327     d wacr            S                   LIKE(acrcro)                         *di lavoro
007700020327     d deprsr          S                   LIKE(acrrsr)
007800020327     d depinr          S                   LIKE(acrinr)
007900020327     d depcar          S                   LIKE(acrcar)
008000020327     d deplor          S                   LIKE(acrlor)
008100020327     d depprr          S                   LIKE(acrprr)
008200020327     d depnar          S                   LIKE(acrnar)
008300020325     d deplnaR         S                   LIKE(o95lna)                         *depositi
008400000000     d deplad          S                   LIKE(o95lna)
008500020325     d NInpO121Cde     S                   LIKE(acrcro)                         *cambi"normalizzati"
008600020325     d NInpO121Rsr     S                   LIKE(InpO121Rsr)
008700020325     d NInpO121Inr     S                   LIKE(InpO121Inr)
008800020325     d NInpO121Nar     S                   LIKE(InpO121Nar)
008900020325     d NInpO121Lor     S                   LIKE(InpO121Lor)
009000020325     d NInpO121Car     S                   LIKE(InpO121Car)
009100020325     d NInpO121Prr     S                   LIKE(InpO121Prr)
009200020325     d NInpO121Dar     S                   LIKE(InpO121Dar)
009300020328     d NInpo121Rer     S                   LIKE(Inpo121Rer)
009400020328     d NInpo121Ter     S                   LIKE(Inpo121Ter)
009500020325     d NInpO121Orr     S                   LIKE(InpO121Orr)
009600020325     d NInpO121AD1     S                   LIKE(InpO121AD1)
009700020325     d NInpO121AA1     S                   LIKE(InpO121AA1)
009800020325     d NInpO121AD2     S                   LIKE(InpO121AD2)
009900020325     d NInpO121AA2     S                   LIKE(InpO121AA2)
010000020325     d NInpO121Rsc     S                   LIKE(InpO121Rsc)
010100020325     d NInpO121Inc     S                   LIKE(InpO121Inc)
010200020325     d NInpO121Nac     S                   LIKE(InpO121Nac)
010300020325     d NInpO121Loc     S                   LIKE(InpO121Loc)
010400020325     d NInpO121Cac     S                   LIKE(InpO121Cac)
010500020325     d NInpO121Prc     S                   LIKE(InpO121Prc)
010600000000     d*---
010700000000     d* Variabili di controlli
010800000000     d*---
010900000000     d $e              S              2  0                                      *indice errori Out
011000000000     d $err            S              1                                         *controllore errore
011100020325     d $OkCde          S              1                                         *validità C.Ritiro
011200000000     d $oknum          S              1                                         *validità numerica
011300060605     D msgId...
011400060605     D                 S              7A
011500060605     D msgDta...
011600060605     D                 S            512A
011700060605     D msg1...
011800060605     D                 S            644A
011900060605     D secLvl...
012000060605     D                 S           3512A
012100060605     D rtnCode...
012200060605     D                 S             10A
012300060605     D esito15...
012400060605     D                 S             15P 0
012500060605     d*---
012600000000     d* Variabili NON riferite al data base
012700000000     d*---
012800000000     d i               S              3  0                                      *indici
012900000000     d ii              S              3  0
013000000000     d n14             S             14  0                                      *Numerico 14
013100000000     d n2              S              2  0                                      *Numerico 2
013200000000     d n3              S              3  0                                      *Numerico 3
013300000000     d n8              S              8  0                                      *Numerico 8
013400000000     d n71             S              7  1                                      *Numerico 7,1
013500000000     d a1              S              1                                         *Alfabetico 1
013600000000     d a3              S              3                                         *Alfabetico 3
013700000000     d a7              S              7                                         *Alfabetico 7
013800000000     d a5              S              5                                         *Alfabetico 5
013900000000     d datcor          S              8  0                                      *data corrente a/m/g
014000000000     d oracor          S              6  0                                      *ora corrente
014100000000     d dardep          S              8  0                                      *dep.data pr.merce
014200020321     d depcli          S              7  0                                      *depositi
014300000000     d werr            S              1                                         *errore generico
014400000000     d wimpmsg         S              1                                         *msg da impostare
014500000000     d wmsg            S             80                                         *msg impostato
014600020325     d tmsg            S              1                                         *tipo msg impostato
014700020321     d wcli            S              7  0                                      *di lavoro
014800000000     d wpgm            S             20                                         *programma
014900000000     d wdat            S              8  0                                      *data (aaaammgg)
015000000000     d wdate           S             10                                         *data (gg/mm/aaaa)
015100000000     d wora            S              4  0                                      *ora  (hhmm)
015200000000     d worae           S              5                                         *ora  (hh:mm)
015300040921     D CmdExcStr       S             50
015400040921     D CmdExcLen       S             15  5
015500040921     D                                     INZ(%SIZE(CmdExcStr))
015600000000     d*---
015700000000     d* Schiere
015800000000     d*---
015900000000     d SErr            S              1    DIM(10)                              *errori          Out
016000000000     d SMsg            S             80    DIM(10)                              *messaggi errori Out
016100020321      * clienti figli
016200020321     d sksc7           S              7  0 DIM(500)                             *clienti figli
016300020321      * clienti ritiro
016400020321     d SCro            S             10S 0 Dim($Dim)                            *clienti ritiro
016500000000      * tabella filiali
016600130423     d*SOrg            S              3  0 DIM(300)                             *filiale
016700130423     d*SOdes           S             30    DIM(300)                             *descrizione filiale
016800130423     d*SOfl1           S              1    DIM(300)                             *flag italia/estero
016900130423     d*SOpt            S              1    DIM(300)                             *flag poste/no poste
017000130423     d*SOdpd           S              1    DIM(300)                             *flag DPD/no DPD
017100000000      * tabella provincie
017200000000     d SPrv            S              2    DIM(200)                             *provincia
017300000000     d SPDes           S             20    DIM(200)                             *descriz.provincia
017400000000      * tabella nazioni
017500000000     d SNaz            S              3    DIM(500)                             *nazione
017600000000     d SNDes           S             25    DIM(500)                             *descrizione nazione
017700000000     d SNLad           S              3  0 DIM(500)                             *l.arrivo DPD
017800000000     d*---
017900000000     d* Parametri I/O
018000000000     d*---
018100020321     d tio121IDs     E Ds                  PREFIX(Inp)                          *Input
018200020321     d tio121ODs     E Ds                  PREFIX(Out)                          *Output
018300130423     D tio121IDSsize   S             10I 0
018400130423     D tio121ODSsize   S             10I 0
018500020327     d tio110IDS     E ds                  PREFIX(Inp)                          *Inp scrittura ORM
018600020327     d tio110ODS     E ds                  PREFIX(Out)                          *Out scrittura ORM
018700000000     d*---
018800000000     d* Ds
018900000000     d*---
019000000000     d* controllo data
019100000000     d wlbda8          DS                  INZ                                  *controlla data
019200000000     d  g08dat                        8  0                                       -data dritta
019300000000     d  g08inv                        8  0                                       -data invertita
019400000000     d  g08err                        1                                          -errore
019500000000     d  g08tgi                        5  0                                       -giorni fra date
019600000000      * scomposizione data generica
019700000000     d dsdat           DS                  INZ                                  *data
019800000000     d  dsann                         4  0                                       -anno
019900000000     d  dsmes                         2  0                                       -mese
020000000000     d  dsgio                         2  0                                       -giorno
020100000000      * scomposizione data generica (anno + mese/giorno)
020200000000     d                 DS                  INZ                                  *data aa + mm/gg
020300000000     d  dsannB                 1      4  0                                       -anno
020400000000     d  dsmgsB                 5      8  0                                       -mese/giorno
020500000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
020600000000      * composizione data generica editata
020700000000     d dsdate          DS                  INZ                                  *data
020800000000     d  dsgioe                        2  0                                       -giorno
020900000000     d  dsvd1                         1                                          -'/'
021000000000     d  dsmese                        2  0                                       -mese
021100000000     d  dsvd2                         1                                          -'/'
021200000000     d  dsanne                        4  0                                       -anno
021300000000      * scomposizione ora generica
021400000000     d dsora           DS                  INZ                                  *ora
021500000000     d  dshh                          2  0                                       -ore
021600000000     d  dsmm                          2  0                                       -minuti
021700000000      * composizione ora generica editata
021800000000     d dsorae          DS                  INZ                                  *ora
021900000000     d  dshhe                         2  0                                       -ore
022000000000     d  dsvo1                         1                                          -':'
022100000000     d  dsmme                         2  0                                       -minuti
022200000000     d* controlla cap
022300000000     d tisi95ds      E DS                                                       *parametri chiamata
022400081119     D tisi97ds      E DS                  INZ
022500081119     D fnlv12ds      E DS                  INZ
022600081119     d* reperimento clienti da cliente unificante
022700020325     d tibs10ds      E DS                                                       *parametri chiamata
022800020325     d  sksc11                21   5520  0 DIM(500)                              -schiera clienti
022900000000      * lettura campo orgde3 di AZORG00F
023000000000     d og143         E DS
023100000000      * DS Provincie
023200000000     d dspr          E DS
023300000000      * DS Nazioni
023400000000     d ds15          E DS
023500090211     d ds3idp        E DS                  QUALIFIED
023600090211     d                                     INZ
023700130423     D azorg00f      E DS                  QUALIFIED TEMPLATE
023800130423     D kpjba         E DS                  QUALIFIED TEMPLATE
023900090211
024000041126     D Esito           S              5I 0
024100041126     D Msg708          S             80
024200041126
024300000000     c*--------------------------------------------------------------------------------------------*
024400000000     c* Main lines
024500000000     c*--------------------------------------------------------------------------------------------*
024600000000     c*
024700000000     c* operazioni iniziali -da fare sempre-
024800000000     c                   EXSR      rutinz
024900020321     c*
025000020321     C* carica in schiere i clienti ritiro dell'utente STRATEGI
025100020321     c                   EXSR      CarAcr
025200020321     c*
025300020325     c* controlla il codice cliente destinatario (fra i codici cliente ritiro)
025400020325     c                   EXSR      ChkInpCde
025500020321     c*
025600020321     c* controlla il nominativo ritiro
025700020321     c                   EXSR      ChkInpRsr
025800020321     c*
025900020321     c* controlla l' indirizzo ritiro
026000020321     c                   EXSR      ChkInpInr
026100020321     c*
026200020321     c* controlla la nazione ritiro
026300020321     c                   EXSR      ChkInpNar
026400020321     c*
026500020321     c* controlla il CAP ritiro
026600020321     c                   EXSR      ChkInpCar
026700020321     c*
026800020321     c* controlla la località ritiro
026900020321     c                   EXSR      ChkInpLor
027000020321     c*
027100020321     c* controlla la provincia ritiro
027200020321     c                   EXSR      ChkInpPrr
027300020321     c*
027400020321     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione ritiro
027500020321     c                   EXSR      ChkInpICKPNr
027600020321     c*
027700020321     c* controlla il peso
027800020321     c                   EXSR      ChkInpPkg
027900020321     c*
028000020321     c* controlla il numero colli
028100020321     c                   EXSR      ChkInpNcl
028200020321     c*
028300020321     c* controlla la data pronta merce
028400041126     c*                  EXSR      ChkInpDar
028500020321     c*
028600020321     c* controlla l' ora pronta merce
028700041126     c*                  EXSR      ChkInpOrr
028800020328     c*
028900041126     c* Controlla data e ora pronta merce.
029000041126     c                   EXSR      ChkDaROrR
029100041126     c*
029200020328     c* controlla la persona da contattare e il suo numero di telefono
029300020328     c                   EXSR      ChkInpRer
029400020321     c*
029500020321     c* controlla il pagamento
029600020321     c                   EXSR      ChkInpPag
029700020321     c*
029800020321     c* controlla la quantita merce
029900020321     c                   EXSR      ChkInpQta
030000000000     c*
030100000000     c* controlla gli orari di apertura UNO
030200000000     c                   EXSR      ChkInpOA1
030300000000     c*
030400000000     c* controlla gli orari di apertura DUE
030500000000     c                   EXSR      ChkInpOA2
030600000000     c*
030700000000     c* controlla gli orari di apertura UNO e DUE
030800000000     c                   EXSR      ChkInpOA1OA2
030900000000     c*
031000000000     c* controlla se ritiro singolo / multiplo
031100000000     c                   EXSR      ChkInpUPD
031200000000     c*
031300000000     c* controlla il destinatario
031400000000     c                   EXSR      ChkInpDes
031500020321     c*
031600000000     c* controlla il fermo deposito
031700000000     c                   EXSR      ChkInpFfd
031800000000     c*
031900000000     c* controlla la scelta del network estero
032000000000     c                   EXSR      ChkInpSse
032100000000     c*
032200020327     c* se non ci sono errori
032300000000if  1c                   IF        $err = '0'                                   *NO errori
032400020327     c*
032500020327     c* imposta i campi della DS di Output
032600000000     c                   EXSR      ImpDsOut
032700020327     c*
032800020327     c* lancia la srittura dell'ORM
032900020327     C                   EXSR      LanWriVpi
033000020327e   1C                   ENDIF                                                  *SI errori
033100020327     c*
033200020327     c* se ci sono errori, porta i relativi messaggi in Output
033300020327if  1c                   IF        $err <> '0'                                  *SI errori
033400020321     c                   MOVEA     SErr          Outo121Err                     *codice
033500020321     c                   MOVEA     SMsg          Outo121Msg                     *messaggio
033600000000e   1c                   ENDIF
033700000000     c*
033800000000     c* operazioni finali
033900000000     c                   EXSR      rutfin
034000000000     c*
034100000000     C                   RETURN
034200020321     c*--------------------------------------------------------------------------------------------*
034300020325     c* controlla il codice cliente destinatario (fra i codici cliente ritiro)
034400020321     c*--------------------------------------------------------------------------------------------*
034500020325     c     ChkInpCde     BEGSR
034600020321     c*
034700020321     c* reimposta le variabili di lavoro
034800020321     c                   MOVEL     '0'           werr                           *NO errore
034900020325     c                   CLEAR                   NInpO121Cde                    *campo"normalizzato"
035000020325     c                   MOVEL     'N'           $OkCde                         *validità
035100020321     c*
035200020321     c* controlla il dato solo se immesso
035300020321do  1c                   DO
035400020325if  2c                   IF        InpO121Cde <> *blanks AND
035500020325     c                             InpO121Cde > *zeros
035600020321     c*
035700020325     c* controlla se il codice cliente destinatario è tutto un numero
035800020321     c                   SETOFF                                       98
035900020325     c                   TESTN                   InpO121Cde           98
036000020321if  3c                   IF        *in98                                        *da TESTN è numero
036100020325     c                   MOVE      InpO121Cde    a1                             *ultimo carattere
036200020321if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
036300020325     c                   MOVEL     'S'           $OkCde                         *valido
036400020321e   4c                   ENDIF
036500020321e   3c                   ENDIF
036600020325if  3c                   IF        $OkCde = 'N'                                 *NON valido
036700020321     c                   MOVEL     '1'           werr                           *errore Output
036800020321     c                   LEAVE
036900020321e   3c                   ENDIF
037000020321     c*
037100020321     c* controlla se il cliente ritiro appartiene a quelli del codice STRATEGI di entrata
037200020321if  3c                   IF        werr = '0'                                   *ancora valido
037300020325     c                   MOVEL     InpO121Cde    NInpO121Cde                    *campo"normalizzato"
037400020321     c                   SETOFF                                           98
037500020321     c                   Z-ADD     1             i
037600020325     c     NInpO121Cde   LOOKUP    SCro(i)                                98
037700020321if  4c                   IF        NOT *in98                                    *NON trovato
037800020321     c                   MOVEL     '1'           werr                           *errore Output
037900020321     c                   LEAVE
038000020321     c*
038100020321     c* legge e controlla il cliente ritiro in anagrafica
038200020321x   4c                   ELSE                                                   *trovato
038300020327     c                   MOVEL     NInpO121Cde   wacr
038400020321     c                   EXSR      LetAcrCro
038500020321e   4c                   ENDIF
038600020321e   3c                   ENDIF
038700020321e   2c                   ENDIF
038800020321     C*
038900020321e   1C                   ENDDO
039000020321     c*
039100020321     c* se errore, memorizza il codice errore nella schiera errori
039200020321if  1c                   IF        werr = '1'
039300020321if  2c                   IF        $e < 10                                      *non + di 10 errori
039400020321     c                   ADD       1             $e
039500020321     c                   MOVEL     '1'           Serr($e)                       *aggiunge errore
039600060605     c*                  MOVEL     msg(1)        Smsg($e)                       *messaggio
039700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0066'
039800060609     C                             :inpO121lang)
039900060609     c                   MOVEL     '1'           $err                           *controllore errore
040000020321e   2c                   ENDIF
040100020321e   1c                   ENDIF
040200020321     c*
040300020321     c                   ENDSR
040400020321     c*--------------------------------------------------------------------------------------------*
040500020321     c* legge e controlla il cliente ritiro in anagrafica
040600020321     c*--------------------------------------------------------------------------------------------*
040700020321     c     LetAcrCro     BEGSR
040800020321     c*
040900020327     c* azzera le variabili di output
041000020327     c                   CLEAR                   deprsr
041100020327     c                   CLEAR                   depinr
041200020327     c                   CLEAR                   depcar
041300020327     c                   CLEAR                   deplor
041400020327     c                   CLEAR                   depprr
041500020327     c                   CLEAR                   depnar
041600020327     c*
041700020327     c* controlla esistenza codice
041800020327     c                   MOVEL     NInpO121Cde   wacr
041900020411     c                   MOVEL     NInpO121Cde   kcrcro
042000020321     c     keyacr        CHAIN     fnacr01l                           98
042100020321if  1c                   IF        NOT *in98        AND                         *esiste
042200020321     c                             acratb = *blanks AND                         *NO annullati
042300020321     c                             acrtcr <> 'M'                                *NO infrequenti
042400020327     c                   EVAL      deprsr = ACRRSR
042500020327     c                   EVAL      depinr = ACRINR
042600020327     c                   EVAL      depcar = ACRCAR
042700020327     c                   EVAL      deplor = ACRLOR
042800020327     c                   EVAL      depprr = ACRPRR
042900020327     c                   EVAL      depnar = ACRNAR
043000020327x   1c                   ELSE                                                   *NON esiste
043100020321     c                   MOVEL     '1'           werr                           *errore
043200020321e   1c                   ENDIF
043300020321     c*
043400020321     c                   ENDSR
043500020321     c*--------------------------------------------------------------------------------------------*
043600020321     c* controlla il nominativo ritiro
043700020321     c*--------------------------------------------------------------------------------------------*
043800020321     c     ChkInpRsr     BEGSR
043900020321     c*
044000020321     c* reimposta le variabili di lavoro
044100020321     c                   MOVEL     '0'           werr                           *NO errore
044200020325     c                   CLEAR                   NInpO121Rsr                    *campo"normalizzato"
044300020325     c*
044400020325     c* campo obbligatorio
044500020321do  1C                   DO
044600020325if  2c                   IF        InpO121Rsr = *blanks
044700020321     c                   MOVEL     '1'           werr                           *SI errore
044800020321     c                   LEAVE
044900020321e   2c                   ENDIF
045000020321     c*
045100020321e   1c                   ENDDO
045200020321     c*
045300020321     c* trasforma il campo immesso da minuscolo a maiuscolo
045400020321     c     minu:maiu     XLATE     InpO121Rsr    NInpO121Rsr                    *Minus -> Maiuscolo
045500020321     C*
045600020321     c*
045700020321     c* se errore, memorizza il codice errore nella schiera errori
045800020321if  1c                   IF        werr = '1'
045900020321if  2c                   IF        $e < 10                                      *non + di 10 errori
046000020321     c                   ADD       1             $e
046100020321     c                   MOVEL     '2'           Serr($e)                       *aggiunge errore
046200060605     c*                  MOVEL     msg(2)        Smsg($e)                       *messaggio
046300060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0482'
046400060609     C                             :inpO121lang)
046500060609     c                   MOVEL     '1'           $err                           *controllore errore
046600020321e   2c                   ENDIF
046700020321e   1c                   ENDIF
046800020321     c*
046900020321     c                   ENDSR
047000020321     c*--------------------------------------------------------------------------------------------*
047100020321     c* controlla l' indirizzo ritiro
047200020321     c*--------------------------------------------------------------------------------------------*
047300020321     c     ChkInpInr     BEGSR
047400020321     c*
047500020321     c* reimposta le variabili di lavoro
047600020321     c                   MOVEL     '0'           werr                           *NO errore
047700020321     c                   CLEAR                   NInpO121Inr                    *campo"normalizzato"
047800020321     c*
047900020321     c* campo obbligatorio se non inerito il codice cliente ritiro
048000020321do  1C                   DO
048100020325if  2c                   IF        InpO121inr = *blanks
048200020321     c                   MOVEL     '1'           werr                           *SI errore
048300020321     c                   LEAVE
048400020321e   2c                   ENDIF
048500020321     C*
048600020321e   1c                   ENDDO
048700020321     c*
048800020321     c* trasforma il campo immesso da minuscolo a maiuscolo
048900020321     c     minu:maiu     XLATE     InpO121Inr    NInpO121Inr                    *Minus -> Maiuscolo
049000020321     c*
049100020321     c* se errore, memorizza il codice errore nella schiera errori
049200020321if  1c                   IF        werr = '1'
049300020321if  2c                   IF        $e < 10                                      *non + di 10 errori
049400020321     c                   ADD       1             $e
049500020321     c                   MOVEL     '3'           Serr($e)                       *aggiunge errore
049600060605     c*                  MOVEL     msg(3)        Smsg($e)                       *messaggio
049700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0283'
049800060609     C                             :inpO121lang)
049900060609     c                   MOVEL     '1'           $err                           *controllore errore
050000020321e   2c                   ENDIF
050100020321e   1c                   ENDIF
050200020321     c*
050300020321     c                   ENDSR
050400020321     c*--------------------------------------------------------------------------------------------*
050500020321     c* controlla il CAP ritiro
050600020321     c*--------------------------------------------------------------------------------------------*
050700020321     c     ChkInpCar     BEGSR
050800020321     c*
050900020321     c* reimposta le variabili di lavoro
051000020321     c                   MOVEL     '0'           werr                           *NO errore
051100020321     c                   CLEAR                   NInpO121Car                    *campo"normalizzato"
051200020321     c*
051300020321     c* campo obbligatorio se non inerito il codice cliente ritiro
051400020321do  1C                   DO
051500020325if  2c                   IF        InpO121Car = *blanks
051600020321     c                   MOVEL     '1'           werr                           *SI errore
051700020321     c                   LEAVE
051800020321e   2c                   ENDIF
051900020321     C*
052000020321e   1c                   ENDDO
052100020321     c*
052200020321     c* trasforma il campo immesso da minuscolo a maiuscolo
052300020321     c     minu:maiu     XLATE     InpO121Car    NInpO121Car                    *Minus -> Maiuscolo
052400020321     c*
052500020321     c* se errore, memorizza il codice errore nella schiera errori
052600020321if  1c                   IF        werr = '1'
052700020321if  2c                   IF        $e < 10                                      *non + di 10 errori
052800020321     c                   ADD       1             $e
052900020321     c                   MOVEL     '4'           Serr($e)                       *aggiunge errore
053000060605     c*                  MOVEL     msg(4)        Smsg($e)                       *messaggio
053100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0053'
053200060609     C                             :inpO121lang)
053300060609     c                   MOVEL     '1'           $err                           *controllore errore
053400020321e   2c                   ENDIF
053500020321e   1c                   ENDIF
053600020321     c*
053700020321     c                   ENDSR
053800020321     c*--------------------------------------------------------------------------------------------*
053900020321     c* controlla la località ritiro
054000020321     c*--------------------------------------------------------------------------------------------*
054100020321     c     ChkInpLor     BEGSR
054200020321     c*
054300020321     c* reimposta le variabili di lavoro
054400020321     c                   MOVEL     '0'           werr                           *NO errore
054500020321     c                   CLEAR                   NInpO121Lor                    *campo"normalizzato"
054600020321     c*
054700020321     c* campo obbligatorio se non inerito il codice cliente ritiro
054800020321do  1C                   DO
054900020325if  2c                   IF        InpO121Lor = *blanks
055000020321     c                   MOVEL     '1'           werr                           *SI errore
055100020321     c                   LEAVE
055200020321e   2c                   ENDIF
055300020321     C*
055400020321e   1c                   ENDDO
055500020321     c*
055600020321     c* trasforma il campo immesso da minuscolo a maiuscolo
055700020321     c     minu:maiu     XLATE     InpO121Lor    NInpO121Lor                    *Minus -> Maiuscolo
055800020321     c*
055900020321     c* se errore, memorizza il codice errore nella schiera errori
056000020321if  1c                   IF        werr = '1'
056100020321if  2c                   IF        $e < 10                                      *non + di 10 errori
056200020321     c                   ADD       1             $e
056300020321     c                   MOVEL     '5'           Serr($e)                       *aggiunge errore
056400060605     c*                  MOVEL     msg(5)        Smsg($e)                       *messaggio
056500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0347'
056600060609     C                             :inpO121lang)
056700060609     c                   MOVEL     '1'           $err                           *controllore errore
056800020321e   2c                   ENDIF
056900020321e   1c                   ENDIF
057000020321     c*
057100020321     c                   ENDSR
057200020321     c*--------------------------------------------------------------------------------------------*
057300020321     c* controlla la provincia ritiro
057400020321     c*--------------------------------------------------------------------------------------------*
057500020321     c     ChkInpPrr     BEGSR
057600020321     c*
057700020321     c* reimposta le variabili di lavoro
057800020321     c                   MOVEL     '0'           werr                           *NO errore
057900020321     c                   CLEAR                   NInpO121Prr                    *campo"normalizzato"
058000020321     c*
058100020321     c* controlla se il dato deve essere immesso oppure no
058200020321do  1c                   DO
058300020321     c*
058400020321     c* trasforma il campo immesso da minuscolo a maiuscolo
058500020321     c     minu:maiu     XLATE     InpO121Prr    NInpO121Prr                    *Minus -> Maiuscolo
058600020321     C*
058700020321     c* controlla esistenza codice in anagrafica
058800020321if  2c                   IF        NInpO121Prr <> *blanks                       *se inserito
058900020321     c                   SETOFF                                           98
059000020321     c                   Z-ADD     1             i
059100020321     c     NInpO121Prr   LOOKUP    SPrv(i)                                98
059200020321if  3c                   IF        NOT *in98                                    *NON trovata
059300020321     c                   MOVEL     '1'           werr                           *SI errore
059400020321     c                   LEAVE
059500020321e   3c                   ENDIF
059600020321e   2c                   ENDIF
059700020321     c*
059800020321e   1C                   ENDDO
059900020321     c*
060000020321     c* se errore, memorizza il codice errore nella schiera errori
060100020321if  1c                   IF        werr = '1'
060200020321if  2c                   IF        $e < 10                                      *non + di 10 errori
060300020321     c                   ADD       1             $e
060400020321     c                   MOVEL     '6'           Serr($e)                       *aggiunge errore
060500060605     c*                  MOVEL     msg(6)        Smsg($e)                       *messaggio
060600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0466'
060700060609     C                             :inpO121lang)
060800060609     c                   MOVEL     '1'           $err                           *controllore errore
060900020321e   2c                   ENDIF
061000020321e   1c                   ENDIF
061100020321     c*
061200020321     c                   ENDSR
061300020321     c*--------------------------------------------------------------------------------------------*
061400020321     c* controlla la nazione ritiro
061500020321     c*--------------------------------------------------------------------------------------------*
061600020321     c     ChkInpNar     BEGSR
061700020321     c*
061800020321     c* reimposta le variabili di lavoro
061900020321     c                   MOVEL     '0'           werr                           *NO errore
062000020321     c                   CLEAR                   NInpO121Nar                    *campo"normalizzato"
062100020321     c*
062200020321     c* controlla se il dato deve essere immesso oppure no
062300020321do  1c                   DO
062400020321     c*
062500020321     c* trasforma il campo immesso da minuscolo a maiuscolo
062600020321     c     minu:maiu     XLATE     InpO121Nar    NInpO121Nar                    *Minus -> Maiuscolo
062700020321     c*
062800020321     c* controlla esistenza codice in anagrafica
062900020321if  2c                   IF        NInpO121Nar <> *blanks                       *se inserito
063000020321     c                   SETOFF                                           98
063100020321     c                   Z-ADD     1             i
063200020321     c     NInpO121Nar   LOOKUP    SNaz(i)                                98
063300020321if  3c                   IF        NOT *in98                                    *NON trovata
063400020321     c                   MOVEL     '1'           werr                           *SI errore
063500020321     c                   LEAVE
063600020321e   3c                   ENDIF
063700020321e   2c                   ENDIF
063800020321     c*
063900020321e   1C                   ENDDO
064000020321     c*
064100020321     c* se errore, memorizza il codice errore nella schiera errori
064200020321if  1c                   IF        werr = '1'
064300020321if  2c                   IF        $e < 10                                      *non + di 10 errori
064400020321     c                   ADD       1             $e
064500020321     c                   MOVEL     '7'           Serr($e)                       *aggiunge errore
064600060605     c*                  MOVEL     msg(7)        Smsg($e)                       *messaggio
064700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0386'
064800060609     C                             :inpO121lang)
064900060609     c                   MOVEL     '1'           $err                           *controllore errore
065000020321e   2c                   ENDIF
065100020321e   1c                   ENDIF
065200020321     c*
065300020321     c                   ENDSR
065400020321     c*--------------------------------------------------------------------------------------------*
065500020321     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione ritiro
065600020321     c*--------------------------------------------------------------------------------------------*
065700020321     c     ChkInpICKPNr  BEGSR
065800020321     c*
065900020321     c* reimposta le variabili di lavoro
066000020321     c                   MOVEL     '0'           werr                           *NO errore
066100020325     c                   Z-ADD     *zeros        deplnaR                        *deposito lna ritiro
066200020321     c*
066300020321     c* controlla se il dato deve essere controllato oppure no
066400020321do  1c                   DO
066500020321     c*
066600020321     c* controlla esattezza dati ritiro
066700020321     C                   CLEAR                   tisi95ds
066800020321     C                   MOVEL     ' '           I95tla
066900020321     C                   MOVEL     '7'           I95tcn
067000020321     C                   MOVEL     NInpO121Nar   I95nar
067100020321     C                   MOVEL     NInpO121Car   I95cap
067200020321     C                   MOVEL     NInpO121Lor   I95loc
067300020321     C                   MOVEL     NInpO121Prr   I95prv
067400020321     C                   MOVEL     NInpO121Inr   I95ind
067500020321     C                   Z-ADD     datcor        I95dat
067600120829     C                   IF        i95nar = IRLANDA
067700120829     C                   EVAL      i95cap = EIRE
067800120829     C                   ENDIF
067900120829     C                   CALL      'TISI95R'
068000020321     C                   PARM                    tisi95ds
068100020321     c*
068200020321     c* se ritorna errore o dato non affidabile imposta errore
068300020325if  2c                   IF        NInpO121Nar = *blanks  AND                   *ITALIA
068400020321     c                             O95Lia <> '3'          OR                    *affidabilità 3
068500020321     c                             NInpO121Nar <> *blanks AND                   *ESTERO
068600020321     c                             O95Lia <> '2'                                *affidabilità 2
068700020321     c                   MOVEL     '1'           werr                           *SI errore
068800020321     c                   LEAVE
068900020325x   2c                   ELSE
069000020325     c                   Z-ADD     O95Lna        depLnaR                        *linea arrivo ritiro
069100020325e   2c                   ENDIF
069200020321     c*
069300020321e   1C                   ENDDO
069400020321     c*
069500020321     c* se errore, memorizza il codice errore nella schiera errori
069600020321if  1c                   IF        werr = '1'
069700020321if  2c                   IF        $e < 10                                      *non + di 10 errori
069800020321     c                   ADD       1             $e
069900020321     c                   MOVEL     '8'           Serr($e)                       *aggiunge errore
070000060605     c*                  MOVEL     msg(8)        Smsg($e)                       *messaggio
070100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0054'
070200060609     C                             :inpO121lang)
070300060609     c                   MOVEL     '1'           $err                           *controllore errore
070400020321e   2c                   ENDIF
070500020321e   1c                   ENDIF
070600020321     C*
070700020321     C                   ENDSR
070800020321     c*--------------------------------------------------------------------------------------------*
070900020321     c* controlla il peso
071000020321     c*--------------------------------------------------------------------------------------------*
071100020321     c     ChkInpPkg     BEGSR
071200020321     c*
071300020321     c* reimposta le variabili di lavoro
071400020321     c                   MOVEL     '0'           werr                           *NO errore
071500020321     c                   CLEAR                   NInpo121Pkg       7 1          *campi"normalizzati"
071600020321     c                   CLEAR                   a7
071700020321     c*
071800020321     c* controlla il campo solo se immesso
071900020321if  1c                   IF        Inpo121Pkg <> *blanks
072000020321     c     ' ' :'0'      XLATE     Inpo121Pkg    a7
072100020321do  2c                   DO
072200020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
072300020321     c                   SETOFF                                       98
072400020321     c                   TESTN                   a7                   98
072500020321if  3c                   IF        *in98                                        *da TESTN è numero
072600020321     c                   MOVE      a7            a1                             *ultimo carattere
072700020321if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
072800020321     c                   MOVEL     'S'           $OkNum                         *valido
072900020321e   4c                   ENDIF
073000020321e   3c                   ENDIF
073100020321if  3c                   IF        $OkNum = 'N'                                 *NON valido
073200020321     c                   MOVEL     '1'           werr                           *errore Output
073300020321     c                   LEAVE
073400020321e   3c                   ENDIF
073500020321     c*
073600020321     c* sposta il campo in quello "normalizzato"
073700020321     c                   MOVEL     a7            NInpo121Pkg                    *campo"normalizzato"
073800020321e   2c                   ENDDO
073900020321e   1c                   ENDIF
074000020321     c*
074100020321     c* se errore, memorizza il codice errore nella schiera errori
074200020321if  1c                   IF        werr = '1'
074300020321if  2c                   IF        $e < 10                                      *non + di 10 errori
074400020321     c                   ADD       1             $e
074500020321     c                   MOVEL     '9'           Serr($e)                       *aggiunge errore
074600060605     c*                  MOVEL     msg(9)        Smsg($e)                       *messaggio
074700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0453'
074800060609     C                             :inpO121lang)
074900060609     c                   MOVEL     '1'           $err                           *controllore errore
075000020321e   2c                   ENDIF
075100020321e   1c                   ENDIF
075200090211     ** Se richiesto il ritiro con DPD
075300090211     ** il peso diventa obbligatorio e deve essere inferiore al limite.
075400090211     C                   IF        wErr = '0' AND
075500090211     C                             inpo121sse = 'D' AND
075600090211     C                             ds3idp.§3iPkd > 0 AND
075700090211     C                             (NInpo121Pkg = 0 OR
075800090211     C                              NInpo121Pkg > ds3idp.§3iPkd)
075900090211     c                   IF        $e < 10                                      *non + di 10 errori
076000090211     c                   ADD       1             $e
076100090211     c                   MOVEL     '9'           Serr($e)                       *aggiunge errore
076200090211     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0694'
076300090211     C                             : inpO121Lang : %EDITC(ds3idp.§3iPkd:'3'))
076400090211     c                   MOVEL     '1'           $err                           *controllore errore
076500090211     c                   ENDIF
076600090211     C                   ENDIF
076700090211     c*
076800020321     c                   ENDSR
076900020321     c*--------------------------------------------------------------------------------------------*
077000020321     c* controlla il numero colli
077100020321     c*--------------------------------------------------------------------------------------------*
077200020321     c     ChkInpNcl     BEGSR
077300020321     c*
077400020321     c* reimposta le variabili di lavoro
077500020321     c                   MOVEL     '0'           werr                           *NO errore
077600020321     c                   CLEAR                   NInpo121Ncl       5 0
077700020321     c*
077800020321     c* controlla il campo solo se immesso
077900020325if  1c                   IF        Inpo121Ncl <> *blanks
078000020325do  2c                   DO
078100020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
078200020321     c                   SETOFF                                       98
078300020321     c                   TESTN                   Inpo121Ncl           98
078400020325if  3c                   IF        *in98                                        *da TESTN è numero
078500020321     c                   MOVE      Inpo121Ncl    a1                             *ultimo carattere
078600020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
078700020321     c                   MOVEL     'S'           $OkNum                         *valido
078800020325e   4c                   ENDIF
078900020325e   3c                   ENDIF
079000020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
079100020321     c                   MOVEL     '1'           werr                           *errore Output
079200020321     c                   LEAVE
079300020325e   3c                   ENDIF
079400020321     c*
079500020321     c* sposta il campo in quello "normalizzato"
079600020321     c                   MOVEL     Inpo121Ncl    NInpo121Ncl                    *campo"normalizzato"
079700020321     c*
079800020325e   2c                   ENDDO
079900020325e   1c                   ENDIF
080000020321     c*
080100020321     c* se errore, memorizza il codice errore nella schiera errori
080200020325if  1c                   IF        werr = '1'
080300020325if  2c                   IF        $e < 10                                      *non + di 10 errori
080400020321     c                   ADD       1             $e
080500020321     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
080600060606     c*                  MOVEL     msg(10)       Smsg($e)                       *messaggio
080700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0410'
080800060609     C                             :inpO121lang)
080900060609     c                   MOVEL     '1'           $err                           *controllore errore
081000020325e   2c                   ENDIF
081100020325e   1c                   ENDIF
081200090211     ** Se richiesto il ritiro con DPD
081300090211     ** i colli diventano obbligatori e devono essere 1.
081400090211     C                   IF        wErr = '0' AND
081500090211     C                             inpo121sse = 'D' AND
081600090211     C                             NInpo121Ncl <> 1
081700090211     c                   IF        $e < 10                                      *non + di 10 errori
081800090211     c                   ADD       1             $e
081900090211     c                   MOVEL     'A'           Serr($e)                       *aggiunge errore
082000090211     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0693'
082100090211     C                             : inpO121Lang)
082200090211     c                   MOVEL     '1'           $err                           *controllore errore
082300090211     c                   ENDIF
082400090211     C                   ENDIF
082500090211     c*
082600020321     c                   ENDSR
082700000000     c*--------------------------------------------------------------------------------------------*
082800000000     c* controlla la data pronta merce
082900000000     c*--------------------------------------------------------------------------------------------*
083000000000     c     ChkInpDar     BEGSR
083100000000     c*
083200000000     c* reimposta le variabili di lavoro
083300000000     c                   MOVEL     '0'           werr                           *NO errore
083400000000     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
083500000000     c                   CLEAR                   wmsg                           *msg impostato
083600020325     c                   CLEAR                   tmsg                           *tipo msg impostato
083700020321     c                   CLEAR                   NInpo121Dar                    *campo"normalizzato"
083800000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
083900000000     c                   CLEAR                   dardep                         *depositi
084000000000     c*
084100000000     c* controlla se la data pronta merce è tutta un numero
084200000000do  1c                   DO
084300000000     c                   SETOFF                                       98
084400020321     c                   TESTN                   Inpo121Dar           98
084500000000if  2c                   IF        *in98                                        *da TESTN è numero
084600020321     c                   MOVE      Inpo121Dar    a1                             *ultimo carattere
084700000000if  3c                   IF        a1 >= '0'                                    *NON trovata lettera
084800000000     c                   MOVEL     'S'           $OkNum                         *valido
084900000000e   3c                   ENDIF
085000000000e   2c                   ENDIF
085100000000if  2c                   IF        $OkNum = 'N'                                 *NON valido
085200000000     c                   MOVEL     '1'           werr                           *errore Output
085300000000     c                   LEAVE
085400000000e   2c                   ENDIF
085500000000     c*
085600000000     c* sposta il campo in quello "normalizzato"
085700020321     c                   MOVEL     Inpo121Dar    NInpo121Dar                    *campo"normalizzato"
085800000000     c*
085900000000     c* controlla validità
086000020321     C                   MOVEL     NInpo121Dar   n8                             *data (8) in g/m/a
086100000000     C                   Z-ADD     n8            g08dat
086200000000     C                   Z-ADD     *zeros        g08inv
086300000000     C                   MOVEL     '0'           g08err
086400000000     c                   CALL      'XSRDA8'
086500000000     c                   PARM                    wlbda8
086600000000     c                   Z-ADD     g08inv        dardep
086700000000if  2c                   IF        g08err = '1'                                 *data errata
086800000000     c                   MOVEL     '1'           werr                           *errore Output
086900000000     c                   LEAVE
087000000000e   2c                   ENDIF
087100000000     c*
087200000000     c* reperimento data + ora corrente
087300000000     C                   TIME                    n14                            *reperimento ora
087400000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
087500000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
087600000000     C                   Z-ADD     n8            g08dat
087700000000     C                   Z-ADD     *zeros        g08inv
087800000000     C                   MOVEL     '0'           g08err
087900000000     c                   CALL      'XSRDA8'
088000000000     c                   PARM                    wlbda8
088100000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
088200000000     c*
088300000000     c* la data non può essere di un giorno PRIMA di oggi
088400000000if  2c                   IF        dardep < datcor
088500000000     c                   MOVEL     '1'           werr                           *errore Output
088600000000     c                   LEAVE
088700000000e   2c                   ENDIF
088800000000     c*
088900000000     c* se il tempo di ritiro del CAP di ritiro è -->
089000041124     c*  - "01" se l'ora corrente è entro le 12.30 allora la data ritiro può essere oggi
089100041124     c*         se l'ora corrente è dopo  le 12.30 allora la data ritiro NON può essere oggi
089200020321     c*  - "02" la data ritiro NON può essere oggi
089300000000if  2c                   IF        dardep = datcor
089400000000     C                   CLEAR                   tisi95ds
089500000000     C                   MOVEL     ' '           I95tla
089600000000     C                   MOVEL     '3'           I95tcn
089700020321     C                   MOVEL     InpO121Nar    I95nar
089800020321     C                   MOVEL     InpO121Car    I95cap
089900020321     C                   MOVEL     InpO121Lor    I95loc
090000020321     C                   MOVEL     InpO121Prr    I95prv
090100000000     C                   Z-ADD     datcor        I95dat
090200120829     C                   IF        i95nar = IRLANDA
090300120829     C                   EVAL      i95cap = EIRE
090400120829     C                   ENDIF
090500120829     C                   CALL      'TISI95R'                                    *reperimento tempo
090600000000     C                   PARM                    tisi95ds
090700000000     c*
090800000000if  3c                   IF        O95gf2 = '01' AND
090900041124     c                             oracor > 123000 OR
091000000000     c                             O95gf2 = '02'
091100000000     c                   MOVEL     '1'           werr                           *errore Output
091200000000     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
091300020325     c                   MOVEL     'C'           tmsg                           *tipo messaggio
091400060606     c*                  MOVEL     msg(12)       wmsg                           *messaggio
091500060609     C                   EVAL      wmsg = rtvMsgLang('TIS0301'
091600060609     C                             :inpO121lang)
091700060609     c                   LEAVE
091800000000e   3c                   ENDIF
091900000000e   2c                   ENDIF
092000000000     c*
092100000000e   1c                   ENDDO
092200000000     c*
092300000000     c* se errore, memorizza il codice errore nella schiera errori
092400000000if  1c                   IF        werr = '1'
092500000000if  2c                   IF        $e < 10                                      *non + di 10 errori
092600000000     c                   ADD       1             $e
092700000000if  3c                   IF        wimpmsg = '0'                                *msg da impostare
092800020325     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
092900060606     c*                  MOVEL     msg(11)       Smsg($e)                       *messaggio
093000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0116'
093100060609     C                             :inpO121lang)
093200060609x   3c                   ELSE                                                   *msg già impostato
093300020325     c                   MOVEL     tmsg          Serr($e)                       *aggiunge errore
093400000000     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
093500000000e   3c                   ENDIF
093600000000     c                   MOVEL     '1'           $err                           *controllore errore
093700000000e   2c                   ENDIF
093800000000e   1c                   ENDIF
093900000000     c*
094000000000     c                   ENDSR
094100000000     c*--------------------------------------------------------------------------------------------*
094200000000     c* controlla l' ora pronta merce
094300000000     c*--------------------------------------------------------------------------------------------*
094400000000     c     ChkInpOrr     BEGSR
094500000000     c*
094600000000     c* reimposta le variabili di lavoro
094700000000     c                   MOVEL     '0'           werr                           *NO errore
094800020321     c                   CLEAR                   NInpo121Orr                    *campo"normalizzato"
094900000000     c                   MOVEL     'N'           $OkNum                         *validità numerica
095000000000     c*
095100000000     c* controlla se l' ora pronta merce è tutta un numero
095200000000do  1c                   DO
095300000000     c                   SETOFF                                       98
095400020321     c                   TESTN                   Inpo121Orr           98
095500000000if  2c                   IF        *in98                                        *da TESTN è numero
095600020321     c                   MOVE      Inpo121Orr    a1                             *ultimo carattere
095700000000if  3c                   IF        a1 >= '0'                                    *NON trovata lettera
095800000000     c                   MOVEL     'S'           $OkNum                         *valido
095900000000e   3c                   ENDIF
096000000000e   2c                   ENDIF
096100000000if  2c                   IF        $OkNum = 'N'                                 *NON valido
096200000000     c                   MOVEL     '1'           werr                           *errore Output
096300000000     c                   LEAVE
096400000000e   2c                   ENDIF
096500000000     c*
096600000000     c* sposta il campo in quello "normalizzato"
096700020321     c                   MOVEL     Inpo121Orr    NInpo121Orr                    *campo"normalizzato"
096800000000     c*
096900000000     c* controlla validità
097000020321     C                   MOVEL     NInpo121Orr   n2                             *hh
097100000000if  2c                   IF        n2 >=  0 AND
097200000000     c                             n2 <= 23
097300000000x   2c                   ELSE
097400000000     c                   MOVEL     '1'           werr                           *errore Output
097500000000     c                   LEAVE
097600000000e   2c                   ENDIF
097700000000     c*
097800020321     C                   MOVE      NInpo121Orr   n2                             *mm
097900000000if  2c                   IF        n2 >=  0 AND
098000000000     c                             n2 <= 59
098100000000x   2c                   ELSE
098200000000     c                   MOVEL     '1'           werr                           *errore Output
098300000000     c                   LEAVE
098400000000e   2c                   ENDIF
098500000000     c*
098600000000e   1c                   ENDDO
098700000000     c*
098800000000     c* se errore, memorizza il codice errore nella schiera errori
098900000000if  1c                   IF        werr = '1'
099000000000if  2c                   IF        $e < 10                                      *non + di 10 errori
099100000000     c                   ADD       1             $e
099200020321     c                   MOVEL     'D'           Serr($e)                       *aggiunge errore
099300060606     c*                  MOVEL     msg(13)       Smsg($e)                       *messaggio
099400060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0423'
099500060609     C                             :inpO121lang)
099600060609     c                   MOVEL     '1'           $err                           *controllore errore
099700000000e   2c                   ENDIF
099800000000e   1c                   ENDIF
099900000000     c*
100000000000     c                   ENDSR
100100041126
100200041126     ***********************************************************************************************
100300041126     **
100400041126     ** Controllo data e ora pronta merce.
100500041126     **
100600041126     ***********************************************************************************************
100700041126     C     ChkDaROrR     BEGSR
100800041126
100900041126     C                   CALL(E)   'TIO708R'
101000041126     C                   PARM                    InpO121RQSCID
101100041126     C                   PARM                    InpO121KSU
101200041126     C                   PARM      *ZERO         CRO708           10
101300041126     C                   PARM                    InpO121CAR
101400041126     C                   PARM                    InpO121LOR
101500041126     C                   PARM                    InpO121PRR
101600041126     C                   PARM                    InpO121NAR
101700041126     C                   PARM                    InpO121DAR
101800041126     C                   PARM                    InpO121ORR
101900041126     C                   PARM      0             Esito
102000041126     C                   PARM      *BLANK        Msg708
102100060606     C                   PARM                    InpO121Lang
102200041126
102300041126     ** se errore, memorizza il codice errore nella schiera errori
102400041126     C                   IF        Esito < 0
102500041126if  2c                   IF        $e < 10                                      *non + di 10 errori
102600041126     c                   ADD       1             $e
102700041126     c                   MOVEL     'B'           Serr($e)                       *aggiunge errore
102800041126     c                   MOVEL     Msg708        Smsg($e)                       *messaggio
102900041126     c                   MOVEL     '1'           $err                           *controllore errore
103000041126e   2c                   ENDIF
103100041203     C                   ELSE
103200041203     c                   MOVEL     Inpo121Dar    NInpo121Dar                    *campo"normalizzato"
103300041203     c                   MOVEL     Inpo121Orr    NInpo121Orr                    *campo"normalizzato"
103400041126e   1c                   ENDIF
103500041126
103600041126     C                   ENDSR
103700041126
103800020328     c*--------------------------------------------------------------------------------------------*
103900020328     c* controlla la persona da contattare e il suo numero di telefono
104000020328     c*--------------------------------------------------------------------------------------------*
104100020328     c     ChkInpRer     BEGSR
104200020328     c*
104300020328     c* reimposta le variabili di lavoro
104400020328     c                   MOVEL     '0'           werr                           *NO errore
104500020328     c                   CLEAR                   NInpo121Rer                    *campi"normalizzati"
104600020328     c                   CLEAR                   NInpo121TeR
104700020328     c*
104800020328     c* campo obbligatorio
104900020328do  1c                   DO
105000020328if  2c                   IF        Inpo121Rer <> *blanks AND
105100020328     c                             Inpo121Ter <> *blanks
105200020328x   2c                   ELSE
105300020328     c                   MOVEL     '1'           werr                           *errore Output
105400020328     c                   LEAVE
105500020328e   2c                   ENDIF
105600020328     c*
105700020328     c* sposta il campo in quello "normalizzato"
105800020328     c     minu:maiu     XLATE     Inpo121Rer    NInpo121Rer                    *Minus -> Maiuscolo
105900020328     c     minu:maiu     XLATE     Inpo121Ter    NInpo121Ter                    *Minus -> Maiuscolo
106000020328e   1c                   ENDDO
106100020328     c*
106200020328     c* se errore, memorizza il codice errore nella schiera errori
106300020328if  1c                   IF        werr = '1'
106400020328if  2c                   IF        $e < 10                                      *non + di 10 errori
106500020328     c                   ADD       1             $e
106600020328     c                   MOVEL     'E'           Serr($e)                       *aggiunge errore
106700060606     c*                  MOVEL     msg(14)       Smsg($e)                       *messaggio
106800060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0451'
106900060609     C                             :inpO121lang)
107000060609     c                   MOVEL     '1'           $err                           *controllore errore
107100020328e   2c                   ENDIF
107200020328e   1c                   ENDIF
107300020328     c*
107400020328     c                   ENDSR
107500020325     c*--------------------------------------------------------------------------------------------*
107600020325     c* controlla il pagamento
107700020325     c*--------------------------------------------------------------------------------------------*
107800020325     c     ChkInpPag     BEGSR
107900020325     c*
108000020325     c* reimposta le variabili di lavoro
108100020325     c                   MOVEL     '0'           werr                           *NO errore
108200020325     c*
108300020325     c* controlla validità scelta
108400020325do  1c                   DO
108500020325if  2c                   IF        Inpo121Pag = 'M' OR
108600020325     c                             Inpo121Pag = 'D' OR
108700020325     c                             Inpo121Pag = 'O'
108800020325x   2c                   ELSE
108900020325     c                   MOVEL     '1'           werr                           *SI errore
109000020325     c                   LEAVE
109100020325e   2c                   ENDIF
109200020325     c*
109300020325e   1C                   ENDDO
109400020325     c*
109500020325     c* se errore, memorizza il codice errore nella schiera errori
109600020325if  1c                   IF        werr = '1'
109700020325if  2c                   IF        $e < 10                                      *non + di 10 errori
109800020325     c                   ADD       1             $e
109900020325     c                   MOVEL     'F'           Serr($e)                       *aggiunge errore
110000060606     c*                  MOVEL     msg(15)       Smsg($e)                       *messaggio
110100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0140'
110200060609     C                             :inpO121lang)
110300060609     c                   MOVEL     '1'           $err                           *controllore errore
110400020325e   2c                   ENDIF
110500020325e   1c                   ENDIF
110600020325     c*
110700020325     c                   ENDSR
110800020321     c*--------------------------------------------------------------------------------------------*
110900020321     c* controlla la quantità merce
111000020321     c*--------------------------------------------------------------------------------------------*
111100020321     c     ChkInpQta     BEGSR
111200020321     c*
111300020321     c* reimposta le variabili di lavoro
111400020321     c                   MOVEL     '0'           werr                           *NO errore
111500020321     c                   CLEAR                   NInpo121Vlm       5 3
111600020321     c                   CLEAR                   NInpo121Bnc       5 0
111700020321     c                   CLEAR                   NInpo121Blc       5 0
111800020321     c                   CLEAR                   NInpo121Att       5 0
111900020321     c                   CLEAR                   NInpo121Mtc       5 0
112000020321     c                   CLEAR                   a5
112100020321     c*---
112200020321     c* Volume
112300020321     c*---
112400020321     c*
112500020321     c* controlla il campo solo se immesso
112600020321if  1c                   IF        werr = '0'
112700020321if  2c                   IF        Inpo121Vlm <> *blanks
112800020321     c     ' ' :'0'      XLATE     Inpo121Vlm    a5
112900020321do  3c                   DO
113000020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
113100020321     c                   SETOFF                                       98
113200020321     c                   TESTN                   a5                   98
113300020321if  4c                   IF        *in98                                        *da TESTN è numero
113400020321     c                   MOVE      a5            a1                             *ultimo carattere
113500020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
113600020321     c                   MOVEL     'S'           $OkNum                         *valido
113700020321e   5c                   ENDIF
113800020321e   4c                   ENDIF
113900020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
114000020321     c                   MOVEL     '1'           werr                           *errore Output
114100020321     c                   LEAVE
114200020321e   4c                   ENDIF
114300020321     c*
114400020321     c*
114500020321     c* sposta il campo in quello "normalizzato"
114600020321     c                   MOVEL     a5            NInpo121Vlm                    *campo"normalizzato"
114700020321e   3c                   ENDDO
114800020321e   2c                   ENDIF
114900020321     c*
115000020321     c* se errore, memorizza il codice errore nella schiera errori
115100020321if  2c                   IF        werr = '1'
115200020321if  3c                   IF        $e < 10                                      *non + di 10 errori
115300020321     c                   ADD       1             $e
115400020325     c                   MOVEL     'G'           Serr($e)                       *aggiunge errore
115500060606     c*                  MOVEL     msg(16)       Smsg($e)                       *messaggio
115600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0610'
115700060609     C                             :inpO121lang)
115800060609     c                   MOVEL     '1'           $err                           *controllore errore
115900020321e   3c                   ENDIF
116000020321e   2c                   ENDIF
116100020321e   1c                   ENDIF
116200020321     c*---
116300020321     c* Numero bancali
116400020321     c*---
116500020321     c*
116600020321     c* controlla il campo solo se immesso
116700020321if  1c                   IF        werr = '0'
116800020321if  2c                   IF        Inpo121Bnc <> *blanks
116900020321do  3c                   DO
117000020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
117100020321     c                   SETOFF                                       98
117200020321     c                   TESTN                   Inpo121Bnc           98
117300020321if  4c                   IF        *in98                                        *da TESTN è numero
117400020321     c                   MOVE      Inpo121Bnc    a1                             *ultimo carattere
117500020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
117600020321     c                   MOVEL     'S'           $OkNum                         *valido
117700020321e   5c                   ENDIF
117800020321e   4c                   ENDIF
117900020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
118000020321     c                   MOVEL     '1'           werr                           *errore Output
118100020321     c                   LEAVE
118200020321e   4c                   ENDIF
118300020321     c*
118400020321     c* sposta il campo in quello "normalizzato"
118500020321     c                   MOVEL     Inpo121Bnc    NInpo121Bnc                    *campo"normalizzato"
118600020321     c*
118700020321e   3c                   ENDDO
118800020321e   2c                   ENDIF
118900020321     c*
119000020321     c* se errore, memorizza il codice errore nella schiera errori
119100020321if  2c                   IF        werr = '1'
119200020321if  3c                   IF        $e < 10                                      *non + di 10 errori
119300020321     c                   ADD       1             $e
119400020325     c                   MOVEL     'H'           Serr($e)                       *aggiunge errore
119500060606     c*                  MOVEL     msg(17)       Smsg($e)                       *messaggio
119600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0408'
119700060609     C                             :inpO121lang)
119800060609     c                   MOVEL     '1'           $err                           *controllore errore
119900020321e   3c                   ENDIF
120000020321e   2c                   ENDIF
120100020321e   1c                   ENDIF
120200020321     c*---
120300020321     c* Peso + Volume + Bancali
120400020321     c*---
120500020321     c*
120600020321     c* occorre immettere almeno un valore fra questi
120700020321if  1c                   IF        werr = '0'
120800020321do  2c                   DO
120900020321if  3c                   IF        Inpo121Pkg > *zeros OR
121000020321     c                             Inpo121Vlm > *zeros  OR
121100020321     c                             Inpo121Bnc > *zeros
121200020321x   3c                   ELSE
121300020321     c                   MOVEL     '1'           werr                           *errore Output
121400020321     c                   LEAVE
121500020321e   3c                   ENDIF
121600020321     c*
121700020321e   2c                   ENDDO
121800020321     c*
121900020321     c* se errore, memorizza il codice errore nella schiera errori
122000020321if  2c                   IF        werr = '1'
122100020321if  3c                   IF        $e < 10                                      *non + di 10 errori
122200020321     c                   ADD       1             $e
122300020325     c                   MOVEL     'I'           Serr($e)                       *aggiunge errore
122400060606     c*                  MOVEL     msg(18)       Smsg($e)                       *messaggio
122500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0235'
122600060609     C                             :inpO121lang)
122700060609     c                   MOVEL     '1'           $err                           *controllore errore
122800020321e   3c                   ENDIF
122900020321e   2c                   ENDIF
123000020321e   1c                   ENDIF
123100020321     c*---
123200020321     c* Bilici
123300020321     c*---
123400020321     c*
123500020321     c* controlla il campo solo se immesso
123600020321if  1c                   IF        werr = '0'
123700020321if  2c                   IF        Inpo121Blc <> *blanks
123800020321do  3c                   DO
123900020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
124000020321     c                   SETOFF                                       98
124100020321     c                   TESTN                   Inpo121Blc           98
124200020321if  4c                   IF        *in98                                        *da TESTN è numero
124300020321     c                   MOVE      Inpo121Blc    a1                             *ultimo carattere
124400020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
124500020321     c                   MOVEL     'S'           $OkNum                         *valido
124600020321e   5c                   ENDIF
124700020321e   4c                   ENDIF
124800020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
124900020321     c                   MOVEL     '1'           werr                           *errore Output
125000020321     c                   LEAVE
125100020321e   4c                   ENDIF
125200020321     c*
125300020321     c* sposta il campo in quello "normalizzato"
125400020321     c                   MOVEL     Inpo121Blc    NInpo121Blc                    *campo"normalizzato"
125500020321     c*
125600020321e   3c                   ENDDO
125700020321e   2c                   ENDIF
125800020321     c*
125900020321     c* se errore, memorizza il codice errore nella schiera errori
126000020321if  2c                   IF        werr = '1'
126100020321if  3c                   IF        $e < 10                                      *non + di 10 errori
126200020321     c                   ADD       1             $e
126300020325     c                   MOVEL     'L'           Serr($e)                       *aggiunge errore
126400060606     c*                  MOVEL     msg(19)       Smsg($e)                       *messaggio
126500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0409'
126600060609     C                             :inpO121lang)
126700060609     c                   MOVEL     '1'           $err                           *controllore errore
126800020321e   3c                   ENDIF
126900020321e   2c                   ENDIF
127000020321e   1c                   ENDIF
127100020321     c*---
127200020321     c* Autotreni
127300020321     c*---
127400020321     c*
127500020321     c* controlla il campo solo se immesso
127600020321if  1c                   IF        werr = '0'
127700020321if  2c                   IF        Inpo121Att <> *blanks
127800020321do  3c                   DO
127900020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
128000020321     c                   SETOFF                                       98
128100020321     c                   TESTN                   Inpo121Att           98
128200020321if  4c                   IF        *in98                                        *da TESTN è numero
128300020321     c                   MOVE      Inpo121Att    a1                             *ultimo carattere
128400020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
128500020321     c                   MOVEL     'S'           $OkNum                         *valido
128600020321e   5c                   ENDIF
128700020321e   4c                   ENDIF
128800020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
128900020321     c                   MOVEL     '1'           werr                           *errore Output
129000020321     c                   LEAVE
129100020321e   4c                   ENDIF
129200020321     c*
129300020321     c* sposta il campo in quello "normalizzato"
129400020321     c                   MOVEL     Inpo121Att    NInpo121Att                    *campo"normalizzato"
129500020321     c*
129600020321e   3c                   ENDDO
129700020321e   2c                   ENDIF
129800020321     c*
129900020321     c* se errore, memorizza il codice errore nella schiera errori
130000020321if  2c                   IF        werr = '1'
130100020321if  3c                   IF        $e < 10                                      *non + di 10 errori
130200020321     c                   ADD       1             $e
130300020325     c                   MOVEL     'M'           Serr($e)                       *aggiunge errore
130400060606     c*                  MOVEL     msg(20)       Smsg($e)                       *messaggio
130500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0407'
130600060609     C                             :inpO121lang)
130700060609     c                   MOVEL     '1'           $err                           *controllore errore
130800020321e   3c                   ENDIF
130900020321e   2c                   ENDIF
131000020321e   1c                   ENDIF
131100020321     c*---
131200020321     c* Motrici
131300020321     c*---
131400020321     c*
131500020321     c* controlla il campo solo se immesso
131600020321if  1c                   IF        werr = '0'
131700020321if  2c                   IF        Inpo121Mtc <> *blanks
131800020321do  3c                   DO
131900020321     c                   MOVEL     'N'           $OkNum                         *validità numerica
132000020321     c                   SETOFF                                       98
132100020321     c                   TESTN                   Inpo121Mtc           98
132200020321if  4c                   IF        *in98                                        *da TESTN è numero
132300020321     c                   MOVE      Inpo121Mtc    a1                             *ultimo carattere
132400020321if  5c                   IF        a1 >= '0'                                    *NON trovata lettera
132500020321     c                   MOVEL     'S'           $OkNum                         *valido
132600020321e   5c                   ENDIF
132700020321e   4c                   ENDIF
132800020321if  4c                   IF        $OkNum = 'N'                                 *NON valido
132900020321     c                   MOVEL     '1'           werr                           *errore Output
133000020321     c                   LEAVE
133100020321e   4c                   ENDIF
133200020321     c*
133300020321     c* sposta il campo in quello "normalizzato"
133400020321     c                   MOVEL     Inpo121Mtc    NInpo121Mtc                    *campo"normalizzato"
133500020321     c*
133600020321e   3c                   ENDDO
133700020321e   2c                   ENDIF
133800020321     c*
133900020321     c* se errore, memorizza il codice errore nella schiera errori
134000020321if  2c                   IF        werr = '1'
134100020321if  3c                   IF        $e < 10                                      *non + di 10 errori
134200020321     c                   ADD       1             $e
134300020325     c                   MOVEL     'N'           Serr($e)                       *aggiunge errore
134400060606     c*                  MOVEL     msg(21)       Smsg($e)                       *messaggio
134500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0411'
134600060609     C                             :inpO121lang)
134700060609     c                   MOVEL     '1'           $err                           *controllore errore
134800020321e   3c                   ENDIF
134900020321e   2c                   ENDIF
135000020321e   1c                   ENDIF
135100020321     c*
135200020321     c                   ENDSR
135300020325     c*--------------------------------------------------------------------------------------------*
135400020325     c* controlla gli orari di apertura UNO
135500020325     c*--------------------------------------------------------------------------------------------*
135600020325     c     ChkInpOA1     BEGSR
135700020325     c*
135800020325     c* reimposta le variabili di lavoro
135900020325     c                   MOVEL     '0'           werr                           *NO errore
136000020325     c                   CLEAR                   NInpo121AD1                    *campo"normalizzato"
136100020325     c                   CLEAR                   NInpo121AA1
136200020325     c*
136300020325     c* controlla i dati solo se immessi
136400020325if  1c                   IF        Inpo121AD1 <> *blanks OR
136500020325     c                             Inpo121AA1 <> *blanks
136600020325do  2c                   DO
136700020325     c*---
136800020325     c* Da
136900020325     c*---
137000020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
137100020325     c                   SETOFF                                       98
137200020325     c                   TESTN                   Inpo121AD1           98
137300020325if  3c                   IF        *in98                                        *da TESTN è numero
137400020325     c                   MOVE      Inpo121AD1    a1                             *ultimo carattere
137500020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
137600020325     c                   MOVEL     'S'           $OkNum                         *valido
137700020325e   4c                   ENDIF
137800020325e   3c                   ENDIF
137900020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
138000020325     c                   MOVEL     '1'           werr                           *errore Output
138100020325     c                   LEAVE
138200020325e   3c                   ENDIF
138300020325     c*
138400020325     c* sposta il campo in quello "normalizzato"
138500020325     c                   MOVEL     Inpo121AD1    NInpo121AD1                    *campo"normalizzato"
138600020325     c*
138700020325     c* controlla validità
138800020325     C                   MOVEL     NInpo121AD1   n2                             *hh
138900020325if  3c                   IF        n2 >=  0 AND
139000020325     c                             n2 <= 23
139100020325x   3c                   ELSE
139200020325     c                   MOVEL     '1'           werr                           *errore Output
139300020325     c                   LEAVE
139400020325e   3c                   ENDIF
139500020325     c*
139600020325     C                   MOVE      NInpo121AD1   n2                             *mm
139700020325if  3c                   IF        n2 >=  0 AND
139800020325     c                             n2 <= 59
139900020325x   3c                   ELSE
140000020325     c                   MOVEL     '1'           werr                           *errore Output
140100020325     c                   LEAVE
140200020325e   3c                   ENDIF
140300020325     c*---
140400020325     c* A
140500020325     c*---
140600020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
140700020325     c                   SETOFF                                       98
140800020325     c                   TESTN                   Inpo121AA1           98
140900020325if  3c                   IF        *in98                                        *da TESTN è numero
141000020325     c                   MOVE      Inpo121AA1    a1                             *ultimo carattere
141100020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
141200020325     c                   MOVEL     'S'           $OkNum                         *valido
141300020325e   4c                   ENDIF
141400020325e   3c                   ENDIF
141500020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
141600020325     c                   MOVEL     '1'           werr                           *errore Output
141700020325     c                   LEAVE
141800020325e   3c                   ENDIF
141900020325     c*
142000020325     c* sposta il campo in quello "normalizzato"
142100020325     c                   MOVEL     Inpo121AA1    NInpo121AA1                    *campo"normalizzato"
142200020325     c*
142300020325     c* controlla validità
142400020325     C                   MOVEL     NInpo121AA1   n2                             *hh
142500020325if  3c                   IF        n2 >=  0 AND
142600020325     c                             n2 <= 23
142700020325x   3c                   ELSE
142800020325     c                   MOVEL     '1'           werr                           *errore Output
142900020325     c                   LEAVE
143000020325e   3c                   ENDIF
143100020325     c*
143200020325     C                   MOVE      NInpo121AA1   n2                             *mm
143300020325if  3c                   IF        n2 >=  0 AND
143400020325     c                             n2 <= 59
143500020325x   3c                   ELSE
143600020325     c                   MOVEL     '1'           werr                           *errore Output
143700020325     c                   LEAVE
143800020325e   3c                   ENDIF
143900020325     c*---
144000020325     c* Da + A
144100020325     c*---
144200020325     c* l'ora A deve essere maggiore dell'ora DA
144300020325if  3c                   IF        NInpo121AA1 < NInpo121AD1
144400020325     c                   MOVEL     '1'           werr                           *errore Output
144500020325     c                   LEAVE
144600020325e   3c                   ENDIF
144700020325     c*
144800020325e   2c                   ENDDO
144900020325e   1c                   ENDIF
145000020325     c*
145100020325     c* se errore, memorizza il codice errore nella schiera errori
145200020325if  1c                   IF        werr = '1'
145300020325if  2c                   IF        $e < 10                                      *non + di 10 errori
145400020325     c                   ADD       1             $e
145500020325     c                   MOVEL     'O'           Serr($e)                       *aggiunge errore
145600060606     c*                  MOVEL     msg(22)       Smsg($e)                       *messaggio
145700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0457'
145800060609     C                             :inpO121lang)
145900060609     c                   MOVEL     '1'           $err                           *controllore errore
146000020325e   2c                   ENDIF
146100020325e   1c                   ENDIF
146200020325     c*
146300020325     c                   ENDSR
146400020325     c*--------------------------------------------------------------------------------------------*
146500020325     c* controlla gli orari di apertura DUE
146600020325     c*--------------------------------------------------------------------------------------------*
146700020325     c     ChkInpOA2     BEGSR
146800020325     c*
146900020325     c* reimposta le variabili di lavoro
147000020325     c                   MOVEL     '0'           werr                           *NO errore
147100020325     c                   CLEAR                   NInpo121AD2                    *campo"normalizzato"
147200020325     c                   CLEAR                   NInpo121AA2
147300020325     c*
147400020325     c* controlla i dati solo se immessi
147500020325if  1c                   IF        Inpo121AD2 <> *blanks OR
147600020325     c                             Inpo121AA2 <> *blanks
147700020325do  2c                   DO
147800020325     c*---
147900020325     c* Da
148000020325     c*---
148100020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
148200020325     c                   SETOFF                                       98
148300020325     c                   TESTN                   Inpo121AD2           98
148400020325if  3c                   IF        *in98                                        *da TESTN è numero
148500020325     c                   MOVE      Inpo121AD2    a1                             *ultimo carattere
148600020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
148700020325     c                   MOVEL     'S'           $OkNum                         *valido
148800020325e   4c                   ENDIF
148900020325e   3c                   ENDIF
149000020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
149100020325     c                   MOVEL     '1'           werr                           *errore Output
149200020325     c                   LEAVE
149300020325e   3c                   ENDIF
149400020325     c*
149500020325     c* sposta il campo in quello "normalizzato"
149600020325     c                   MOVEL     Inpo121AD2    NInpo121AD2                    *campo"normalizzato"
149700020325     c*
149800020325     c* controlla validità
149900020325     C                   MOVEL     NInpo121AD2   n2                             *hh
150000020325if  3c                   IF        n2 >=  0 AND
150100020325     c                             n2 <= 23
150200020325x   3c                   ELSE
150300020325     c                   MOVEL     '1'           werr                           *errore Output
150400020325     c                   LEAVE
150500020325e   3c                   ENDIF
150600020325     c*
150700020325     C                   MOVE      NInpo121AD2   n2                             *mm
150800020325if  3c                   IF        n2 >=  0 AND
150900020325     c                             n2 <= 59
151000020325x   3c                   ELSE
151100020325     c                   MOVEL     '1'           werr                           *errore Output
151200020325     c                   LEAVE
151300020325e   3c                   ENDIF
151400020325     c*---
151500020325     c* A
151600020325     c*---
151700020325     c                   MOVEL     'N'           $OkNum                         *validità numerica
151800020325     c                   SETOFF                                       98
151900020325     c                   TESTN                   Inpo121AA2           98
152000020325if  3c                   IF        *in98                                        *da TESTN è numero
152100020325     c                   MOVE      Inpo121AA2    a1                             *ultimo carattere
152200020325if  4c                   IF        a1 >= '0'                                    *NON trovata lettera
152300020325     c                   MOVEL     'S'           $OkNum                         *valido
152400020325e   4c                   ENDIF
152500020325e   3c                   ENDIF
152600020325if  3c                   IF        $OkNum = 'N'                                 *NON valido
152700020325     c                   MOVEL     '1'           werr                           *errore Output
152800020325     c                   LEAVE
152900020325e   3c                   ENDIF
153000020325     c*
153100020325     c* sposta il campo in quello "normalizzato"
153200020325     c                   MOVEL     Inpo121AA2    NInpo121AA2                    *campo"normalizzato"
153300020325     c*
153400020325     c* controlla validità
153500020325     C                   MOVEL     NInpo121AA2   n2                             *hh
153600020325if  3c                   IF        n2 >=  0 AND
153700020325     c                             n2 <= 23
153800020325x   3c                   ELSE
153900020325     c                   MOVEL     '1'           werr                           *errore Output
154000020325     c                   LEAVE
154100020325e   3c                   ENDIF
154200020325     c*
154300020325     C                   MOVE      NInpo121AA2   n2                             *mm
154400020325if  3c                   IF        n2 >=  0 AND
154500020325     c                             n2 <= 59
154600020325x   3c                   ELSE
154700020325     c                   MOVEL     '1'           werr                           *errore Output
154800020325     c                   LEAVE
154900020325e   3c                   ENDIF
155000020325     c*---
155100020325     c* Da + A
155200020325     c*---
155300020325     c* l'ora A deve essere maggiore dell'ora DA
155400020325if  3c                   IF        NInpo121AA2 < NInpo121AD2
155500020325     c                   MOVEL     '1'           werr                           *errore Output
155600020325     c                   LEAVE
155700020325e   3c                   ENDIF
155800020325     c*
155900020325e   2c                   ENDDO
156000020325e   1c                   ENDIF
156100020325     c*
156200020325     c* se errore, memorizza il codice errore nella schiera errori
156300020325if  1c                   IF        werr = '1'
156400020325if  2c                   IF        $e < 10                                      *non + di 10 errori
156500020325     c                   ADD       1             $e
156600020325     c                   MOVEL     'P'           Serr($e)                       *aggiunge errore
156700060606     c*                  MOVEL     msg(23)       Smsg($e)                       *messaggio
156800060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0541'
156900060609     C                             :inpO121lang)
157000060609     c                   MOVEL     '1'           $err                           *controllore errore
157100020325e   2c                   ENDIF
157200020325e   1c                   ENDIF
157300020325     c*
157400020325     c                   ENDSR
157500020325     c*--------------------------------------------------------------------------------------------*
157600020325     c* controlla gli orari di apertura UNO e DUE
157700020325     c*--------------------------------------------------------------------------------------------*
157800020325     c     ChkInpOA1OA2  BEGSR
157900020325     c*
158000020325     c* reimposta le variabili di lavoro
158100020325     c                   MOVEL     '0'           werr                           *NO errore
158200020325     c*
158300020325     c* se immessi primi e secondi occorre controllarne la congruità
158400020325if  1c                   IF        Inpo121AD1 <> *blanks AND
158500020325     c                             Inpo121AD2 <> *blanks
158600020325do  2c                   DO
158700020325     c*
158800020325if  3c                   IF        NInpo121AD2 < NInpo121AA1
158900020325     c                   MOVEL     '1'           werr                           *errore Output
159000020325     c                   LEAVE
159100020325e   3c                   ENDIF
159200020325     c*
159300020325e   2c                   ENDDO
159400020325e   1c                   ENDIF
159500020325     c*
159600020325     c* se errore, memorizza il codice errore nella schiera errori
159700020325if  1c                   IF        werr = '1'
159800020325if  2c                   IF        $e < 10                                      *non + di 10 errori
159900020325     c                   ADD       1             $e
160000020325     c                   MOVEL     'Q'           Serr($e)                       *aggiunge errore
160100060606     c*                  MOVEL     msg(24)       Smsg($e)                       *messaggio
160200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0171'
160300060609     C                             :inpO121lang)
160400060609     c                   MOVEL     '1'           $err                           *controllore errore
160500020325e   2c                   ENDIF
160600020325e   1c                   ENDIF
160700020325     c*
160800020325     c                   ENDSR
160900000000     c*--------------------------------------------------------------------------------------------*
161000000000     c* controlla consegna a uno o più destinatari
161100000000     c*--------------------------------------------------------------------------------------------*
161200000000     c     ChkInpUpd     BEGSR
161300000000     c*
161400000000     c* reimposta le variabili di lavoro
161500000000     c                   MOVEL     '0'           werr                           *NO errore
161600020325     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
161700000000     c                   CLEAR                   wmsg                           *msg impostato
161800020325     c                   CLEAR                   tmsg                           *tipo msg impostato
161900000000     c*
162000000000do  1c                   DO
162100000000     c*
162200000000     c* controlla validità campo
162300020321if  2c                   IF        Inpo121Upd = '1' OR
162400020321     c                             Inpo121Upd = '2'
162500000000x   2c                   ELSE
162600000000     c                   MOVEL     '1'           werr                           *errore Output
162700000000     c                   LEAVE
162800000000e   2c                   ENDIF
162900020325     c*
163000020326     c* se la consegna è a più di un destinatario, NON immettere il codice destinatario
163100020325if  2c                   IF        InpO121Upd <> '1' AND
163200020325     c                             NInpO121Cde > *zeros
163300020325     c                   MOVEL     '1'           werr                           *errore Output
163400020325     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
163500020325     c                   MOVEL     'S'           tmsg                           *tipo messaggio
163600060606     c*                  MOVEL     msg(26)       wmsg                           *messaggio
163700060609     C                   EVAL      wmsg = rtvMsgLang('TIS0291'
163800060609     C                             :inpO121lang)
163900060609     c                   LEAVE
164000020325e   2c                   ENDIF
164100101201     c*
164200101201     c* Se DPD, DEVE essere UN destinatario
164300101201if  2c                   IF        InpO121sse = 'D' AND
164400101201     c                             InpO121Upd <> '1'
164500101201     c                   MOVEL     '1'           werr                           *errore Output
164600101201     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
164700101201     c                   MOVEL     'S'           tmsg                           *tipo messaggio
164800101201     C                   EVAL      wmsg = rtvMsgLang('TIS0307'
164900101201     C                             :inpO121Lang)
165000101201     c                   LEAVE
165100101201e   2c                   ENDIF
165200101201     c*
165300000000e   1c                   ENDDO
165400000000     c*
165500000000     c* se errore, memorizza il codice errore nella schiera errori
165600000000if  1c                   IF        werr = '1'
165700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
165800000000     c                   ADD       1             $e
165900020325if  3c                   IF        wimpmsg = '0'                                *msg da impostare
166000020325     c                   MOVEL     'R'           Serr($e)                       *aggiunge errore
166100060606     c*                  MOVEL     msg(25)       Smsg($e)                       *messaggio
166200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0295'
166300060609     C                             :inpO121lang)
166400060609x   3c                   ELSE
166500020325     c                   MOVEL     tmsg          Serr($e)                       *aggiunge errore
166600020325     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
166700020325e   2c                   ENDIF
166800000000     c                   MOVEL     '1'           $err                           *controllore errore
166900000000e   2c                   ENDIF
167000000000e   1c                   ENDIF
167100000000     c*
167200000000     c                   ENDSR
167300000000     c*--------------------------------------------------------------------------------------------*
167400000000     c* controlla il destinatario
167500000000     c*--------------------------------------------------------------------------------------------*
167600000000     c     ChkInpDes     BEGSR
167700000000     c*
167800000000     c* reimposta le variabili di lavoro
167900000000     c                   MOVEL     '0'           werr                           *NO errore
168000020325     c                   MOVEL     '0'           wimpmsg                        *msg da impostare
168100020325     c                   CLEAR                   wmsg                           *msg impostato
168200020325     c                   CLEAR                   tmsg                           *tipo msg impostato
168300020325     c*
168400020325     c* se la consegna è singola il destinatario è da inserire (codice o nome)
168500020325do  1c                   DO
168600020325if  2c                   IF        Inpo121Upd = '1'                             *singola
168700020325if  3c                   IF        (Inpo121Cde = *blanks OR
168800020325     c                             Inpo121Cde = *zeros) AND
168900020325     c                             (Inpo121Rsc = *blanks OR
169000020325     c                             Inpo121Inc = *blanks  OR
169100020325     c                             Inpo121Loc = *blanks  OR
169200020325     c                             Inpo121Cac = *blanks)
169300020325     c                   MOVEL     '1'           werr                           *errore Output
169400020325     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
169500020325     c                   MOVEL     'T'           tmsg                           *tipo messaggio
169600060606     c*                  MOVEL     msg(27)       wmsg                           *messaggio
169700060609     C                   EVAL      wmsg = rtvMsgLang('TIS0289'
169800060609     C                             :inpO121lang)
169900060609e   3c                   ENDIF
170000020325e   2c                   ENDIF
170100020325     c*
170200020326     c* se la consegna è a più di un destinatario, NON immettere il nome destinatario
170300020326     c* (la NON immissione del codice destinatario è già stato controllata sopra)
170400020325if  2c                   IF        InpO121Upd <> '1'
170500020325if  3c                   IF        InpO121Rsc > *blanks OR
170600020325     c                             Inpo121Inc > *blanks OR
170700020325     c                             Inpo121Loc > *blanks OR
170800020325     c                             Inpo121Cac > *blanks
170900020325     c                   MOVEL     '1'           werr                           *errore Output
171000020325     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
171100020325     c                   MOVEL     'U'           tmsg                           *tipo messaggio
171200060606     c*                  MOVEL     msg(28)       wmsg                           *messaggio
171300060609     C                   EVAL      wmsg = rtvMsgLang('TIS0292'
171400060609     C                             :inpO121lang)
171500060609     c                   LEAVE
171600020325e   3c                   ENDIF
171700020325e   2c                   ENDIF
171800020328     c*
171900020328     c* se è già stato inserito il codice destinatario, il nominativo per esteso è da lasciare vuoto
172000020328if  2c                   IF         InpO121Cde  > *zeros  AND
172100020328     c                             (Inpo121Rsc <> *blanks OR
172200020328     c                              Inpo121Inc <> *blanks OR
172300020328     c                              Inpo121Loc <> *blanks OR
172400020328     c                              Inpo121Cac <> *blanks)
172500020328     c                   MOVEL     '1'           werr                           *errore Output
172600020328     c                   MOVEL     '1'           wimpmsg                        *msg già impostato
172700020328     c                   MOVEL     '£'           tmsg                           *tipo messaggio
172800060606     c*                  MOVEL     msg(38)       wmsg                           *messaggio
172900060609     C                   EVAL      wmsg = rtvMsgLang('TIS0294'
173000060609     C                             :inpO121lang)
173100060609e   2c                   ENDIF
173200000000     C*
173300000000e   1C                   ENDDO
173400000000     c*
173500000000     c* se errore, memorizza il codice errore nella schiera errori
173600000000if  1c                   IF        werr = '1'
173700000000if  2c                   IF        $e < 10                                      *non + di 10 errori
173800000000     c                   ADD       1             $e
173900020325     c                   MOVEL     tmsg          Serr($e)                       *aggiunge errore
174000020325     c                   MOVEL     wmsg          Smsg($e)                       *messaggio
174100000000     c                   MOVEL     '1'           $err                           *controllore errore
174200000000e   2c                   ENDIF
174300000000e   1c                   ENDIF
174400020329     c*---
174500020329     c* Dati del destinatario
174600020329     c*---
174700020329if  1c                   IF        Inpo121Upd = '1' AND                         *singola e
174800020329     c                             InpO121Cde <= *zeros                         *codice NON inserito
174900000000     c*---
175000000000     c* Nominativo
175100000000     c*---
175200000000if  2c                   IF        werr <> '1'                                  *NO errori
175300000000     c                   EXSR      ChkInpRsc
175400000000e   2c                   ENDIF
175500000000     c*---
175600000000     c* Indirizzo
175700000000     c*---
175800000000if  2c                   IF        werr <> '1'                                  *NO errori
175900000000     c                   EXSR      ChkInpInc
176000000000e   2c                   ENDIF
176100000000     c*---
176200000000     c* Nazione
176300000000     c*---
176400000000if  2c                   IF        werr <> '1'                                  *NO errori
176500000000     c                   EXSR      ChkInpNac
176600000000e   2c                   ENDIF
176700020325     c*---
176800020325     c* Località
176900020325     c*---
177000020325if  2c                   IF        werr <> '1'                                  *NO errori
177100020325     c                   EXSR      ChkInpLoc
177200020325e   2c                   ENDIF
177300000000     c*---
177400000000     c* CAP
177500000000     c*---
177600000000if  2c                   IF        werr <> '1'                                  *NO errori
177700000000     c                   EXSR      ChkInpCac
177800000000e   2c                   ENDIF
177900000000     c*---
178000000000     c* provincia
178100000000     c*---
178200000000if  2c                   IF        werr <> '1'                                  *NO errori
178300000000     c                   EXSR      ChkInpPrc
178400000000e   2c                   ENDIF
178500000000     c*---
178600000000     c* Indirizzo + il CAP + la località + la provincia + la nazione
178700000000     c*---
178800000000if  2c                   IF        werr <> '1'                                  *NO errori
178900000000     c                   EXSR      ChkInpICKPNc
179000000000e   2c                   ENDIF
179100000000e   1c                   ENDIF
179200000000     c*
179300000000     c                   ENDSR
179400000000     c*--------------------------------------------------------------------------------------------*
179500000000     c* controlla il nominativo destinatario
179600000000     c*--------------------------------------------------------------------------------------------*
179700000000     c     ChkInpRsc     BEGSR
179800000000     c*
179900000000     c* reimposta le variabili di lavoro
180000000000     c                   MOVEL     '0'           werr                           *NO errore
180100020321     c                   CLEAR                   NInpo121Rsc                    *campo"normalizzato"
180200020326     c*
180300020326     c* controlla l'obbligatorietà del campo
180400020326do  1c                   DO
180500020326if  2c                   IF        Inpo121Rsc = *blanks
180600020326     c                   MOVEL     '1'           werr                           *SI errore
180700020326     c                   LEAVE
180800020326e   2c                   ENDIF
180900020326     C*
181000020326e   1c                   ENDDO
181100000000     c*
181200000000     c* trasforma il campo immesso da minuscolo a maiuscolo
181300020321     c     minu:maiu     XLATE     Inpo121Rsc    NInpo121Rsc                    *Minus -> Maiuscolo
181400020326     c*
181500020326     c* se errore, memorizza il codice errore nella schiera errori
181600020326if  1c                   IF        werr = '1'
181700020326if  2c                   IF        $e < 10                                      *non + di 10 errori
181800020326     c                   ADD       1             $e
181900020326     c                   MOVEL     'V'           Serr($e)                       *aggiunge errore
182000060606     c*                  MOVEL     msg(29)       Smsg($e)                       *messaggio
182100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0480'
182200060609     C                             :inpO121lang)
182300060609     c                   MOVEL     '1'           $err                           *controllore errore
182400020326e   2c                   ENDIF
182500020326e   1c                   ENDIF
182600000000     c*
182700000000     c                   ENDSR
182800000000     c*--------------------------------------------------------------------------------------------*
182900000000     c* controlla l'indirizzo destinatario
183000000000     c*--------------------------------------------------------------------------------------------*
183100000000     c     ChkInpInc     BEGSR
183200000000     c*
183300000000     c* reimposta le variabili di lavoro
183400000000     c                   MOVEL     '0'           werr                           *NO errore
183500020321     c                   CLEAR                   NInpo121Inc                    *campo"normalizzato"
183600020326     c*
183700020326     c* controlla l'obbligatorietà del campo
183800020326do  1c                   DO
183900020326if  2c                   IF        Inpo121Inc = *blanks
184000020326     c                   MOVEL     '1'           werr                           *SI errore
184100020326     c                   LEAVE
184200020326e   2c                   ENDIF
184300020326     c*
184400020326e   1C                   ENDDO
184500000000     c*
184600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
184700020321     c     minu:maiu     XLATE     Inpo121Inc    NInpo121Inc                    *Minus -> Maiuscolo
184800020326     c*
184900020326     c* se errore, memorizza il codice errore nella schiera errori
185000020326if  1c                   IF        werr = '1'
185100020326if  2c                   IF        $e < 10                                      *non + di 10 errori
185200020326     c                   ADD       1             $e
185300020326     c                   MOVEL     'Z'           Serr($e)                       *aggiunge errore
185400060606     c*                  MOVEL     msg(30)       Smsg($e)                       *messaggio
185500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0281'
185600060609     C                             :inpO121lang)
185700060609     c                   MOVEL     '1'           $err                           *controllore errore
185800020326e   2c                   ENDIF
185900020326e   1c                   ENDIF
186000000000     c*
186100000000     c                   ENDSR
186200000000     c*--------------------------------------------------------------------------------------------*
186300000000     c* controlla la nazione destinatario
186400000000     c*--------------------------------------------------------------------------------------------*
186500000000     c     ChkInpNac     BEGSR
186600000000     c*
186700000000     c* reimposta le variabili di lavoro
186800000000     c                   MOVEL     '0'           werr                           *NO errore
186900020321     c                   CLEAR                   NInpo121Nac                    *campo"normalizzato"
187000000000     c                   CLEAR                   deplad                         *deposito
187100000000     c*
187200000000     c* trasforma il campo immesso da minuscolo a maiuscolo
187300000000do  1c                   DO
187400020321     c     minu:maiu     XLATE     Inpo121Nac    NInpo121Nac                    *Minus -> Maiuscolo
187500000000     c*
187600000000     c* controlla esistenza codice in anagrafica
187700020326if  2c                   IF        NInpo121Nac <> *blanks                       *se inserito
187800000000     c                   SETOFF                                           98
187900000000     c                   Z-ADD     1             i
188000020321     c     NInpo121Nac   LOOKUP    SNaz(i)                                98
188100000000if  3c                   IF        NOT *in98                                    *NON trovata
188200000000     c                   MOVEL     '1'           werr                           *SI errore
188300000000     c                   LEAVE
188400000000x   3c                   ELSE
188500000000     C                   Z-ADD     SNLad(i)      deplad                         *l.arrivo DPD
188600000000e   3c                   ENDIF
188700000000e   2c                   ENDIF
188800000000     C*
188900000000e   1c                   ENDDO
189000000000     c*
189100000000     c* se errore, memorizza il codice errore nella schiera errori
189200000000if  1c                   IF        werr = '1'
189300000000if  2c                   IF        $e < 10                                      *non + di 10 errori
189400000000     c                   ADD       1             $e
189500020325     c                   MOVEL     'K'           Serr($e)                       *aggiunge errore
189600060606     c*                  MOVEL     msg(34)       Smsg($e)                       *messaggio
189700060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0384'
189800060609     C                             :inpO121lang)
189900060609     c                   MOVEL     '1'           $err                           *controllore errore
190000000000e   2c                   ENDIF
190100000000e   1c                   ENDIF
190200000000     c*
190300000000     c                   ENDSR
190400000000     c*--------------------------------------------------------------------------------------------*
190500000000     c* controlla il CAP destinatario
190600000000     c*--------------------------------------------------------------------------------------------*
190700000000     c     ChkInpCac     BEGSR
190800000000     c*
190900000000     c* reimposta le variabili di lavoro
191000000000     c                   MOVEL     '0'           werr                           *NO errore
191100020321     c                   CLEAR                   NInpo121cac                    *campo"normalizzato"
191200020326     c*
191300020326     c* controlla l'obbligatorietà del campo
191400020326do  1c                   DO
191500020326if  2c                   IF        Inpo121cac = *blanks
191600020326     c                   MOVEL     '1'           werr                           *SI errore
191700020326     c                   LEAVE
191800020326e   2c                   ENDIF
191900020326     c*
192000020326e   1C                   ENDDO
192100000000     C*
192200000000     c* trasforma il campo immesso da minuscolo a maiuscolo
192300020321     c     minu:maiu     XLATE     Inpo121Cac    NInpo121Cac                    *Minus -> Maiuscolo
192400020326     c*
192500020326     c* se errore, memorizza il codice errore nella schiera errori
192600020326if  1c                   IF        werr = '1'
192700020326if  2c                   IF        $e < 10                                      *non + di 10 errori
192800020326     c                   ADD       1             $e
192900020326     c                   MOVEL     'X'           Serr($e)                       *aggiunge errore
193000060606     c*                  MOVEL     msg(31)       Smsg($e)                       *messaggio
193100060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0051'
193200060609     C                             :inpO121lang)
193300060609     c                   MOVEL     '1'           $err                           *controllore errore
193400020326e   2c                   ENDIF
193500020326e   1c                   ENDIF
193600000000     c*
193700000000     c                   ENDSR
193800000000     c*--------------------------------------------------------------------------------------------*
193900000000     c* controlla la località destinatario
194000000000     c*--------------------------------------------------------------------------------------------*
194100000000     c     ChkInpLoc     BEGSR
194200000000     c*
194300000000     c* reimposta le variabili di lavoro
194400000000     c                   MOVEL     '0'           werr                           *NO errore
194500020321     c                   CLEAR                   NInpo121Loc                    *campo"normalizzato"
194600020326     c*
194700020326     c* controlla l'obbligatorietà del campo
194800020326do  1c                   DO
194900020326if  2c                   IF        Inpo121Inc = *blanks
195000020326     c                   MOVEL     '1'           werr                           *SI errore
195100020326     c                   LEAVE
195200020326e   2c                   ENDIF
195300020326     c*
195400020326e   1C                   ENDDO
195500000000     C*
195600000000     c* trasforma il campo immesso da minuscolo a maiuscolo
195700020321     c     minu:maiu     XLATE     Inpo121Loc    NInpo121Loc                    *Minus -> Maiuscolo
195800020326     c*
195900020326     c* se errore, memorizza il codice errore nella schiera errori
196000020326if  1c                   IF        werr = '1'
196100020326if  2c                   IF        $e < 10                                      *non + di 10 errori
196200020326     c                   ADD       1             $e
196300020326     c                   MOVEL     'Y'           Serr($e)                       *aggiunge errore
196400060606     c*                  MOVEL     msg(32)       Smsg($e)                       *messaggio
196500060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0345'
196600060609     C                             :inpO121lang)
196700060609     c                   MOVEL     '1'           $err                           *controllore errore
196800020326e   2c                   ENDIF
196900020326e   1c                   ENDIF
197000000000     c*
197100000000     c                   ENDSR
197200000000     c*--------------------------------------------------------------------------------------------*
197300000000     c* controlla la provincia destinatario
197400000000     c*--------------------------------------------------------------------------------------------*
197500000000     c     ChkInpPrc     BEGSR
197600000000     c*
197700000000     c* reimposta le variabili di lavoro
197800000000     c                   MOVEL     '0'           werr                           *NO errore
197900020321     c                   CLEAR                   NInpo121Prc                    *campo"normalizzato"
198000000000     c*
198100000000     c* controlla l'obbligatorietà del campo
198200000000do  1c                   DO
198300000000     c*
198400000000     c* trasforma il campo immesso da minuscolo a maiuscolo
198500020321     c     minu:maiu     XLATE     Inpo121Prc    NInpo121Prc                    *Minus -> Maiuscolo
198600000000     c*
198700000000     c* controlla esistenza codice in anagrafica
198800020321if  2c                   IF        NInpo121Prc <> *blanks                       *se inserito
198900000000     c                   SETOFF                                           98
199000000000     c                   Z-ADD     1             i
199100020321     c     NInpo121Prc   LOOKUP    SPrv(i)                                98
199200000000if  3c                   IF        NOT *in98                                    *NON trovata
199300000000     c                   MOVEL     '1'           werr                           *SI errore
199400000000     c                   LEAVE
199500000000e   3c                   ENDIF
199600000000e   2c                   ENDIF
199700000000     c*
199800000000e   1C                   ENDDO
199900000000     c*
200000000000     c* se errore, memorizza il codice errore nella schiera errori
200100000000if  1c                   IF        werr = '1'
200200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
200300000000     c                   ADD       1             $e
200400020325     c                   MOVEL     'J'           Serr($e)                       *aggiunge errore
200500060606     c*                  MOVEL     msg(33)       Smsg($e)                       *messaggio
200600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0461'
200700060609     C                             :inpO121lang)
200800060609     c                   MOVEL     '1'           $err                           *controllore errore
200900000000e   2c                   ENDIF
201000000000e   1c                   ENDIF
201100000000     c*
201200000000     c                   ENDSR
201300000000     c*--------------------------------------------------------------------------------------------*
201400000000     c* controlla l' indirizzo + il CAP + la località + la provincia + la nazione destinatario
201500000000     c*--------------------------------------------------------------------------------------------*
201600000000     c     ChkInpICKPNc  BEGSR
201700000000     c*
201800000000     c* reimposta le variabili di lavoro
201900000000     c                   MOVEL     '0'           werr                           *NO errore
202000000000     c*
202100020325     c* controlla esattezza dati destinatario, se inseriti
202200000000do  1c                   DO
202300000000     C                   CLEAR                   tisi95ds
202400000000     C                   MOVEL     ' '           I95tla
202500000000     C                   MOVEL     '7'           I95tcn
202600020321     C                   MOVEL     NInpo121Nac   I95nar
202700020321     C                   MOVEL     NInpo121Cac   I95cap
202800020321     C                   MOVEL     NInpo121Loc   I95loc
202900020321     C                   MOVEL     NInpo121Prc   I95prv
203000020321     C                   MOVEL     NInpo121Inc   I95ind
203100000000     C                   Z-ADD     datcor        I95dat
203200120829     C                   IF        i95nar = IRLANDA
203300120829     C                   EVAL      i95cap = EIRE
203400120829     C                   ENDIF
203500120829     C                   CALL      'TISI95R'
203600000000     C                   PARM                    tisi95ds
203700000000     c*
203800000000     c* se ritorna errore o dato non affidabile imposta errore
203900000000if  2c                   IF        O95Err <> *blanks
204000000000     c                   MOVEL     '1'           werr                           *SI errore
204100000000     c                   LEAVE
204200000000x   2c                   ELSE                                                   *NO errore
204300020321if  3c                   IF        NInpo121Nac = *blanks  AND                   *ITALIA
204400000000     c                             O95Lia <> '3'          OR                    *affidabilità 3
204500020321     c                             NInpo121Nac <> *blanks AND                   *ESTERO
204600000000     c                             O95Lia <> '2'                                *affidabilità 2
204700000000     c                   MOVEL     '1'           werr                           *SI errore
204800000000     c                   LEAVE
204900000000e   3c                   ENDIF
205000000000e   2c                   ENDIF
205100000000     c*
205200000000e   1c                   ENDDO
205300000000     c*
205400000000     c* se errore, memorizza il codice errore nella schiera errori
205500000000if  1c                   IF        werr = '1'
205600000000if  2c                   IF        $e < 10                                      *non + di 10 errori
205700000000     c                   ADD       1             $e
205800020325     c                   MOVEL     'W'           Serr($e)                       *aggiunge errore
205900060606     c*                  MOVEL     msg(35)       Smsg($e)                       *messaggio
206000060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0139'
206100060609     C                             :inpO121lang)
206200060609     c                   MOVEL     '1'           $err                           *controllore errore
206300000000e   2c                   ENDIF
206400000000e   1c                   ENDIF
206500000000     C*
206600000000     C                   ENDSR
206700000000     c*--------------------------------------------------------------------------------------------*
206800000000     c* controlla consegna a uno o più destinatari
206900000000     c*--------------------------------------------------------------------------------------------*
207000000000     c     ChkInpFfd     BEGSR
207100000000     c*
207200000000     c* reimposta le variabili di lavoro
207300000000     c                   MOVEL     '0'           werr                           *NO errore
207400000000     c*
207500000000     c* controlla validità campo
207600000000do  1c                   DO
207700020321if  2c                   IF        Inpo121Ffd = 'S' OR
207800020321     c                             Inpo121Ffd = 'N'
207900000000x   2c                   ELSE
208000000000     c                   MOVEL     '1'           werr                           *errore Output
208100000000     c                   LEAVE
208200000000e   2c                   ENDIF
208300000000     c*
208400000000e   1c                   ENDDO
208500000000     c*
208600000000     c* se errore, memorizza il codice errore nella schiera errori
208700000000if  1c                   IF        werr = '1'
208800000000if  2c                   IF        $e < 10                                      *non + di 10 errori
208900000000     c                   ADD       1             $e
209000020325     c                   MOVEL     '!'           Serr($e)                       *aggiunge errore
209100060606     c*                  MOVEL     msg(36)       Smsg($e)                       *messaggio
209200060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0537'
209300060609     C                             :inpO121lang)
209400060609     c                   MOVEL     '1'           $err                           *controllore errore
209500000000e   2c                   ENDIF
209600000000e   1c                   ENDIF
209700000000     c*
209800000000     c                   ENDSR
209900000000     c*--------------------------------------------------------------------------------------------*
210000000000     c* controlla la scelta del network estero
210100000000     c*--------------------------------------------------------------------------------------------*
210200000000     c     ChkInpSse     BEGSR
210300000000     c*
210400000000     c* reimposta le variabili di lavoro
210500000000     c                   MOVEL     '0'           werr                           *NO errore
210600000000     c*
210700000000     c* controlla validità campo
210800000000do  1c                   DO
210900020321if  2c                   IF        Inpo121Sse = 'N' OR
211000020321     c                             Inpo121Sse = 'E' OR
211100081119     c                             Inpo121Sse = 'D' OR
211200081119     c                             Inpo121Sse = *BLANK
211300081119x   2c                   ELSE
211400000000     c                   MOVEL     '1'           werr                           *errore Output
211500000000     c                   LEAVE
211600000000e   2c                   ENDIF
211700000000     c*
211800000000e   1c                   ENDDO
211900000000     c*
212000000000     c* se errore, memorizza il codice errore nella schiera errori
212100000000if  1c                   IF        werr = '1'
212200000000if  2c                   IF        $e < 10                                      *non + di 10 errori
212300000000     c                   ADD       1             $e
212400020325     c                   MOVEL     '"'           Serr($e)                       *aggiunge errore
212500060606     c*                  MOVEL     msg(37)       Smsg($e)                       *messaggio
212600060609     C                   EVAL      Smsg($e) = rtvMsgLang('TIS0536'
212700060609     C                             :inpO121lang)
212800060609     c                   MOVEL     '1'           $err                           *controllore errore
212900000000e   2c                   ENDIF
213000081119     c                   LEAVESR
213100081119e   1c                   ENDIF
213200081119     ** Verifica la ritirabilità all'estero.
213300081119     C                   IF        inpo121nar <> *BLANK AND
213400081119     C                             inpo121sse <> *BLANK AND inpo121sse <> 'N'
213500081119     C                   RESET                   fnlv12ds
213600081119     C                   RESET                   tisi95ds
213700081119     C                   RESET                   tisi97ds
213800081119     C                   EVAL      i12lang = inpo121lang
213900081119     C                   EVAL      i95nar = inpo121nar
214000081119     C                   EVAL      i95cap = inpo121car
214100081119     C                   EVAL      i95dat = %DEC(%DATE(inpo121dar : *EUR0))
214200081119     C                   IF        inpo121sse = 'D'
214300081119     C                   EVAL      i97ntw = 'DPD'
214400081119     C                   ELSEIF    inpo121sse = 'E'
214500081119     C                   EVAL      i97ntw = 'EEX'
214600081119     C                   ENDIF
214700081119     C                   CALL      'FNLV12R'
214800081119     C                   PARM                    fnlv12ds
214900081119     C                   PARM                    tisi95ds
215000081119     C                   PARM                    tisi97ds
215100081119     C                   IF        o12err <> *BLANK AND $e < 10
215200081119     C                   EVAL      $e += 1
215300081119     C                   EVAL      sErr($e) = 'R'
215400081119     C                   EVAL      sMsg($e) = o12msg
215500081119     C                   EVAL      $err = '1'
215600081119     C                   ENDIF
215700081119     C                   ENDIF
215800081119     c*
215900000000     c                   ENDSR
216000000000     c*--------------------------------------------------------------------------------------------*
216100000000     c* edita data da aaaammgg in gg/mm/aaaa
216200000000     c*--------------------------------------------------------------------------------------------*
216300000000     c     edtdat        BEGSR
216400000000     c*
216500000000     c                   MOVEL     *blanks       wdate
216600000000     c*
216700000000     c                   MOVEL     wdat          dsdat
216800000000     c                   MOVEL     dsgio         dsgioe
216900000000     c                   MOVEL     '/'           dsvd1
217000000000     c                   MOVEL     dsmes         dsmese
217100000000     c                   MOVEL     '/'           dsvd2
217200000000     c                   MOVEL     dsann         dsanne
217300000000     c                   MOVEL     dsdate        wdate
217400000000     c*
217500000000     c                   ENDSR
217600000000     c*--------------------------------------------------------------------------------------------*
217700000000     c* edita ora da hhmm in hh:mm
217800000000     c*--------------------------------------------------------------------------------------------*
217900000000     c     edtora        BEGSR
218000000000     c*
218100000000     c                   MOVEL     *blanks       worae
218200000000     c*
218300000000     c                   MOVEL     wora          dsora
218400000000     c                   MOVEL     dshh          dshhe
218500000000     c                   MOVEL     ':'           dsvo1
218600000000     c                   MOVEL     dsmm          dsmme
218700000000     c                   MOVEL     dsorae        worae
218800000000     c*
218900000000     c                   ENDSR
219000000000     c*--------------------------------------------------------------------------------------------*
219100000000     c* imposta i campi della DS di Output
219200000000     c*--------------------------------------------------------------------------------------------*
219300000000     c     ImpDsOut      BEGSR
219400000000     c*******************
219500000000     c* Note 1      -> Orari di apertura
219600000000     c*******************
219700020321if  1c                   IF        Inpo121AD1 <> *blanks OR
219800020321     c                             Inpo121AD2 <> *blanks
219900020321     c                   EVAL      OutO121No1 =  'Ap'
220000020321if  2c                   IF        Inpo121AD1 <> *blanks
220100020321     c                   EVAL      OutO121No1 =  %TRIM(OutO121No1) + ' ' +
220200020321     c                             NInpO121AD1 + ' ' + NInpO121AA1
220300000000e   2c                   ENDIF
220400020321if  2c                   IF        Inpo121AD2 <> *blanks
220500020321     c                   EVAL      OutO121No1 =  %TRIM(OutO121No1) + ' ' +
220600020321     c                             NInpO121AD2 + ' ' + NInpO121AA2
220700000000e   2c                   ENDIF
220800000000e   1c                   ENDIF
220900020325     c*******************
221000020325     c* PO emittente --> PO cliente PASSWORD
221100020325     c*******************
221200020325     c                   EVAL      OutO121Poe = %SUBST(InpO121Ksu:2:3)
221300020325     c*******************
221400020325     c* PO ritiro --> PO calcolato da CAP di ritiro, ma se scelto servizio DPD occorre prendere il
221500020325     c*               PO di arrivo DPD dalla nazione di ritiro
221600020325     c*******************
221700020325     c                   MOVEL     deplnaR       OutO121Por
221800020325if  1c                   IF        InpO121Sse = 'D' AND                         *servizio DPD
221900020325     c                             InpO121Nar <> *blanks                        *ritiro Estero
222000020325     c                   SETOFF                                           98
222100020325     c                   Z-ADD     1             i
222200020325     c     Inpo121Nar    LOOKUP    SNaz(i)                                98
222300020325if  2c                   IF        *in98                                        *NON trovata
222400020325     c                   MOVEL     SNLad(i)      OutO121Por
222500020325e   2c                   ENDIF
222600020325e   1c                   ENDIF
222700130423     ** Imposto i campi di output nuovi solo se ho ricevuto un parametro lungo
222800130423     ** quanto la nuova struttura dati.
222900130423     C                   IF        %PARMS() > 3 AND
223000130423     C                             %SIZE(tio121ODs) = tio121ODSsize
223100130423     C                   CALLP     Organigramma_getDescrizioneFiliale(
223200130423     C                             %DEC(OutO121Por:3:0) : OutO121PorDes )
223300130423     C                   CALLP     Organigramma_getRecapitiFiliale(
223400130423     C                             %DEC(OutO121Por:3:0) : OutO121PorFon
223500130423     C                             : *OMIT : *OMIT : OutO121PorEma )
223600130423     C                   ENDIF
223700020325     c*******************
223800020325     c* PO consegna -> sempre uguale a zero
223900020325     c*******************
224000020325     c                   MOVEL     *zeros        OutO121Poc                     *NON calcolato
224100020326     c*******************
224200020326     c* Riferimento calcolato -> sempre uguale a zero
224300020326     c*******************
224400020326     c                   MOVEL     InpO121Rfa    OutO121Rfa
224500000000     c*
224600000000     c                   ENDSR
224700020327     c*--------------------------------------------------------------------------------------------*
224800020327     c* lancia la srittura dell'ORM
224900020327     c*--------------------------------------------------------------------------------------------*
225000020327     c     LanWriVpi     BEGSR
225100020327     c*
225200020327     c* azzera DS di passaggio
225300020327     c                   CLEAR                   tio110IDS
225400020327     c                   CLEAR                   tio110ODS
225500020327     c*
225600020327     c* imposta la DS di Input per lanciare il programma che scrive gli ORM
225700020327     c                   EVAL      InpO110RqscId =  Inpo121RqscId
225800020327     c                   EVAL      InpO110Ksu    =  Inpo121Ksu
225900050207     c                   EVAL      InpO110Cro    = *ZERO
226000020327     c                   EVAL      InpO110Rsr    = NInpO121Rsr
226100020327     c                   EVAL      InpO110Inr    = NInpO121Inr
226200020327     c                   EVAL      InpO110Nar    = NInpO121Nar
226300020327     c                   EVAL      InpO110Lor    = NInpO121Lor
226400020327     c                   EVAL      InpO110Car    = NInpO121Car
226500020327     c                   EVAL      InpO110Prr    = NInpO121Prr
226600020327     c                   EVAL      InpO110Pag    =  InpO121Pag
226700020327     c                   EVAL      InpO110Por    =  OutO121Por
226800020327     c                   EVAL      InpO110Poe    =  OutO121Poe
226900020327     c                   EVAL      InpO110Dar    = NInpo121Dar
227000020327     c                   EVAL      InpO110Orr    = NInpo121Orr
227100020327     c                   EVAL      InpO110AD1    = NInpo121AD1
227200020327     c                   EVAL      InpO110AA1    = NInpo121AA1
227300020327     c                   EVAL      InpO110AD2    = NInpo121AD2
227400020327     c                   EVAL      InpO110AA2    = NInpo121AA2
227500020327     c                   MOVEL     NInpo121Pkg   InpO110Pkg
227600020327     c                   MOVEL     NInpo121Vlm   InpO110Vlm
227700020327     c                   MOVEL     NInpo121Bnc   InpO110Bnc
227800020327     c                   MOVEL     NInpo121Ncl   InpO110Ncl
227900020327     c                   MOVEL     NInpo121Blc   InpO110Blc
228000020327     c                   MOVEL     NInpo121Att   InpO110Att
228100020327     c                   MOVEL     NInpo121Mtc   InpO110Mtc
228200020327     c                   EVAL      InpO110Nam    =  Inpo121Nam
228300020328     c                   EVAL      InpO110Rer    = NInpo121Rer
228400020328     c                   EVAL      InpO110Ter    = NInpo121Ter
228500050317     c                   EVAL      InpO110CttTlf =  Inpo121CttTlf
228600020327     c                   EVAL      InpO110Rfa    =  Inpo121Rfa
228700020327     c                   EVAL      InpO110Upd    =  Inpo121Upd
228800020327     c*---
228900020327     c* destinatario -->
229000020327     c* se     immesso il codice destinatario lo decodifica e riempe i campi del destinatario
229100020327     c* se NON immesso il codice destinatario i campi sono quelli immessi a video
229200020327     c*---
229300020329IF  1c                   IF        NInpO121Cde > *zeros
229400050207     c                   EVAL      Inpo110Crc    =  InpO121Cde
229500020327     c                   EVAL      Inpo110Rsc    =  deprsr
229600020327     c                   EVAL      Inpo110Inc    =  depinr
229700020327     c                   EVAL      Inpo110Nac    =  depnar
229800020327     c                   EVAL      Inpo110Loc    =  deplor
229900020327     c                   EVAL      Inpo110Cac    =  depcar
230000020327     c                   EVAL      Inpo110Prc    =  depprr
230100020327x   1c                   ELSE                                                   *NON immesso
230200050207     c                   EVAL      Inpo110Crc    =  *ZERO
230300020327     c                   EVAL      Inpo110Rsc    =  NInpo121Rsc
230400020327     c                   EVAL      Inpo110Inc    =  NInpo121Inc
230500020327     c                   EVAL      Inpo110Nac    =  NInpo121Nac
230600020327     c                   EVAL      Inpo110Loc    =  NInpo121Loc
230700020327     c                   EVAL      Inpo110Cac    =  NInpo121Cac
230800020327     c                   EVAL      Inpo110Prc    =  NInpo121Prc
230900020327e   1c                   ENDIF
231000020327     c                   EVAL      InpO110Ffd    =  Inpo121Ffd
231100020327     c                   EVAL      InpO110Sse    =  Inpo121Sse
231200020327     c                   EVAL      InpO110No2    =  Inpo121No2
231300020327     c                   EVAL      InpO110Poc    =  OutO121Poc
231400020327     c                   EVAL      InpO110Pr2    =  OutO121Por
231500020327     c                   EVAL      InpO110Pe2    =  OutO121Poe
231600020327     c                   EVAL      InpO110No1    =  OutO121No1
231700060606     c                   EVAL      InpO110Lang   =  InpO121Lang
231800040922     C                   CALL(E)   'TIO710R'
231900020327     c                   PARM                    tio110IDS
232000020327     c                   PARM                    tio110ODS
232100040922     c                   MOVEL     Outo110Err    SErr(1)                        *codice
232200040922     c                   MOVEL     Outo110Msg    SMsg(1)                        *messaggio
232300020328     c* SI errori
232400020329if  1C                   IF        %error OR OutO110ERR = '1'
232500020327     c                   MOVEL     '1'           $err                           *errore
232600020329x   1c                   ELSE
232700020329     c* NO errori
232800020329     c                   MOVEL     OutO110Rfa    OutO121Rfa                     *n.riferim.CALCOLATO
232900020327e   1c                   ENDIF
233000020327     C*
233100020327     c                   ENDSR
233200000000     c*--------------------------------------------------------------------------------------------*
233300000000     c* caricamento tabelle occorrenti
233400000000     c*--------------------------------------------------------------------------------------------*
233500000000     c     cartab        BEGSR
233600000000     c*---
233700000000     c* organigramma
233800000000     c*---
233900130423     c*                  CLEAR                   i                              *indice
234000130423     C*                  CLEAR                   sorg
234100130423     C*                  CLEAR                   sodes
234200130423     C*                  CLEAR                   sofl1
234300130423     C*                  CLEAR                   sopt
234400130423     C*                  CLEAR                   sodpd
234500130423     c*    *loval        SETLL     azorg01l
234600130423     c*                  READ      azorg01l                               99
234700130423do  1c*                  DOW       NOT *in99
234800130423     c*                  ADD       1             i
234900130423     c*                  Z-ADD     orgfil        sorg(i)
235000130423     c*                  MOVEL     orgdes        sodes(i)
235100130423     c*                  MOVEL     orgfl1        sofl1(i)
235200130423     c*                  MOVEL     orgde3        og143
235300130423     c*                  MOVEL     §ogpt         sopt(i)
235400130423     c*                  MOVEL     §ogdpd        sodpd(i)
235500130423     c*                  READ      azorg01l                               99
235600130423e   1c*                  ENDDO
235700000000     c*---
235800000000     c* provincie
235900000000     c*---
236000000000     c                   CLEAR                   i                              *indice
236100000000     C                   CLEAR                   dspr
236200000000     C                   CLEAR                   SPrv
236300000000     C                   CLEAR                   SPDes
236400020906     C                   Z-ADD     1             kblkut
236500000000     C                   MOVEL     'PR'          kblcod
236600000000     c     keytab        SETLL     tabel00f
236700000000     c     keytab        READE     tabel00f                               99
236800000000do  1c                   DOW       NOT *in99
236900000000if  2c                   IF        tblflg = *blanks
237000000000     c                   MOVEL     tbluni        dspr
237100000000     c                   ADD       1             i
237200000000     c                   MOVEL     tblkey        SPrv(i)
237300000000     c                   MOVEL     §Prdes        SPDes(i)
237400000000e   2c                   ENDIF
237500000000     c     keytab        READE     tabel00f                               99
237600000000e   1c                   ENDDO
237700000000     c*---
237800000000     c* nazioni
237900000000     c*---
238000000000     c                   CLEAR                   i                              *indice
238100000000     C                   CLEAR                   ds15
238200000000     C                   CLEAR                   SNaz
238300000000     C                   CLEAR                   SNDes
238400020906     C                   Z-ADD     1             kblkut
238500000000     C                   MOVEL     '15'          kblcod
238600000000     c     keytab        SETLL     tabel00f
238700000000     c     keytab        READE     tabel00f                               99
238800000000do  1c                   DOW       NOT *in99
238900000000if  2c                   IF        tblflg = *blanks
239000000000     c                   MOVEL     tbluni        ds15
239100000000     c                   ADD       1             i
239200000000     c                   MOVEL     tblkey        SNaz(i)
239300000000     c                   MOVEL     §15des        SNDes(i)
239400000000     c                   Z-ADD     §15lad        SNLad(i)
239500000000e   2c                   ENDIF
239600000000     c     keytab        READE     tabel00f                               99
239700000000e   1c                   ENDDO
239800090211     c*---
239900090211     c* Regole ritiro DPD.
240000090211     c*---
240100090211     c                   EVAL      kblkut = 1
240200090211     c                   EVAL      kblcod = '3I'
240300090211     c                   EVAL      kblkey = 'DPD'
240400090211     c     keyTab3       CHAIN     tabel00f
240500090211     c                   IF        %FOUND() AND tblFlg = *BLANK
240600090211     c                   EVAL      ds3idp = tblUni
240700090211     c                   ELSE
240800090211     c                   CLEAR                   ds3idp
240900090211     c                   ENDIF
241000090211     c*
241100000000     c                   ENDSR
241200000000     c*--------------------------------------------------------------------------------------------*
241300000000     c* operazioni finali
241400000000     c*--------------------------------------------------------------------------------------------*
241500000000     c     rutfin        BEGSR
241600000000     c*
241700000000     c* chiude pgm
241800000000     C                   CLEAR                   tisi95ds
241900000000     C                   MOVEL     'C'           I95tla
242000000000     C                   CALL      'TISI95R'                                    *controlla CAP
242100000000     C                   PARM                    tisi95ds
242200000000     c*
242300000000     c                   ENDSR
242400000000     c*--------------------------------------------------------------------------------------------*
242500000000     c* operazioni iniziali -da fare sempre-
242600000000     c*--------------------------------------------------------------------------------------------*
242700000000     c     rutinz        BEGSR
242800000000     c*
242900000000     c* reperimento data corrente
243000000000     C                   TIME                    n14                            *ora (6)+ data (8)
243100020321     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
243200000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
243300000000     C                   Z-ADD     n8            g08dat
243400000000     C                   Z-ADD     *zeros        g08inv
243500000000     C                   MOVEL     '0'           g08err
243600000000     c                   CALL      'XSRDA8'
243700000000     c                   PARM                    wlbda8
243800000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
243900000000     c*
244000000000     c* azzera la DS di Output e le variabili di lavro
244100020321     c                   CLEAR                   tio121ODs                      *Ds Output
244200020321     c                   MOVEL     *ALL'0'       Outo121Err                     *NO errore
244300000000     c                   MOVEL     *all'0'       SErr                           *schiera errori
244400000000     c                   MOVEL     *blanks       SMsg                           *schiera msg errori
244500000000     c                   Z-ADD     *zeros        $e                             *indice  errori
244600000000     c                   MOVEL     '0'           $err                           *controllore errore
244700000000     c*
244800000000     c* azzera le variabili "normalizzate"
244900020325     c                   CLEAR                   NInpo121Cde
245000020321     c                   CLEAR                   NInpo121Rsr
245100020321     c                   CLEAR                   NInpo121Inr
245200020321     c                   CLEAR                   NInpo121Nar
245300020321     c                   CLEAR                   NInpo121Lor
245400020321     c                   CLEAR                   NInpo121Car
245500020321     c                   CLEAR                   NInpo121Prr
245600020321     c                   CLEAR                   NInpo121Pkg
245700020321     c                   CLEAR                   NInpo121Ncl
245800020321     c                   CLEAR                   NInpo121Dar
245900020321     c                   CLEAR                   NInpo121Orr
246000020328     c                   CLEAR                   NInpo121Rer
246100020328     c                   CLEAR                   NInpo121Ter
246200020321     c                   CLEAR                   NInpo121Vlm
246300020321     c                   CLEAR                   NInpo121Bnc
246400020321     c                   CLEAR                   NInpo121Blc
246500020321     c                   CLEAR                   NInpo121Att
246600020321     c                   CLEAR                   NInpo121Mtc
246700020321     c                   CLEAR                   NInpo121AD1
246800020321     c                   CLEAR                   NInpo121AA1
246900020321     c                   CLEAR                   NInpo121AD2
247000020321     c                   CLEAR                   NInpo121AA2
247100020321     c                   CLEAR                   NInpo121Rsc
247200020321     c                   CLEAR                   NInpo121Inc
247300020321     c                   CLEAR                   NInpo121Nac
247400020321     c                   CLEAR                   NInpo121Loc
247500020321     c                   CLEAR                   NInpo121Cac
247600020321     c                   CLEAR                   NInpo121Prc
247700000000     c*
247800000000     c                   ENDSR
247900000000     c*--------------------------------------------------------------------------------------------*
248000000000     c* Operazioni iniziali -solo la prima volta-
248100000000     c*--------------------------------------------------------------------------------------------*
248200000000     C     *InzSr        BEGSR
248300000000     c*
248400000000     c* ricevimento parametri
248500000000     c     *ENTRY        PLIST
248600020321     c                   PARM                    tio121IDS
248700020321     c                   PARM                    tio121ODS
248800130423     c                   PARM                    tio121IDSsize
248900130423     c                   PARM                    tio121ODSsize
249000000000     c*
249100000000     c* chiavi di lettura
249200000000     c     keytab        KLIST                                                  tabel00f
249300000000     c                   KFLD                    kblkut                         -utente
249400000000     c                   KFLD                    kblcod                         -tabella
249500090211     c     keyTab3       KLIST                                                  tabel00f
249600090211     c                   KFLD                    kblkut                         -utente
249700090211     c                   KFLD                    kblcod                         -tabella
249800090211     c                   KFLD                    kblkey                         -elemento
249900090211     c     keytbe        KLIST                                                  *tntbe01l
250000000000     c                   KFLD                    kbecod                          -tabella
250100000000     c                   KFLD                    kbeke1                          -chiave uno
250200000000     c                   KFLD                    kbeke2                          -chiave due
250300000000     c                   KFLD                    kbelin                          -lingua
250400000000     c                   KFLD                    kbesif                          -s.informativo
250500000000     c     ke2tbe        KLIST                                                  *tntbe02l
250600000000     c                   KFLD                    kbecod                          -tabella
250700000000     c                   KFLD                    kbelin                          -lingua
250800000000     c                   KFLD                    kbesif                          -s.informativo
250900020321     c     keyvss        KLIST                                                  *tivss02l
251000020321     c                   KFLD                    kssksu                          -cl.unificante
251100020321     c                   KFLD                    kssisv                          -tipo servizio
251200020321     c     keyacr        KLIST                                                  *fnacr01l
251300020321     c                   KFLD                    kcrcro                          -cl.ritiro
251400000000     c*
251500020327     c* aggiunge le librerie in lista
251600041126     c*                  EVAL      wpgm = 'GAITRAOBJ/TIS777C'
251700041126     c*                  CALL      wpgm
251800000000     c*
251900000000     c* apre i files
252000020321     c                   OPEN      tivss02l
252100020321     c                   OPEN      fnacr01l
252200000000     c                   OPEN      tabel00f
252300000000     c                   OPEN      tntbe01l
252400000000     c                   OPEN      tntbe02l
252500130423     c*                  OPEN      azorg01l
252600000000     c*
252700000000     c* caricamento tabelle occorrenti
252800000000     c                   EXSR      cartab
252900000000     c*
253000130423     C                   CALLP     Organigramma_init()
253100130423     c*
253200000000     c                   ENDSR
253300020321     c*--------------------------------------------------------------------------------------------*
253400020321     C* Memorizza in schiere i clienti ritiro dell'utente STRATEGI
253500020321     c*--------------------------------------------------------------------------------------------*
253600020321     C     CarAcr        BEGSR
253700020321     C*
253800020321     C* azzera le variabili di lavoro
253900020321     c                   CLEAR                   i                              *indici
254000020321     c                   CLEAR                   ii
254100020321     c                   CLEAR                   SCro                           *codici clienti
254200020321     C*
254300020321     c* carica i clienti figli legati al cliente unificante di entrata
254400020321     c                   EXSR      CarKun
254500020321     C*
254600020321     C* per ciascun cliente figlio memorizzato (o solo il padre se non ci sono figli)
254700020321     c                   Z-ADD     1             i
254800020321     c                   Z-ADD     *zeros        ii
254900020321do  1C                   DOW       sksc7(i) > *zeros AND                        *piena schiera INP
255000020321     C                             i  <= 500         AND                        *fine  schiera INP
255100020321     C                             ii <  $dim                                   *fine  schiera OUT
255200020321     C*
255300020321     C* memorizza tutti i suoi clienti ritiro + progressivo
255400020321     c                   MOVEL     sksc7(i)      wacr
255500020321     c                   MOVE      000           wacr
255600020321     c                   Z-ADD     wacr          kcrcro
255700020321     c     keyacr        SETLL     fnacr01l
255800020321     c                   READ      fnacr01l                               98
255900020321     c*
256000020321     c* memorizza il cliente ritiro letto (senza prg) in una variabile di lavoro
256100020321     c                   MOVEL     acrcro        wcli                           *cliente senza prg
256200020321     c*
256300020321     c* ciclo fino a fine file o rottura cliente ritiro (senza prg)
256400020321do  2C                   DOW       ii <  $dim        AND                        *fine  schiera OUT
256500020321     c                             NOT *in98         AND                        *fine file
256600020321     c                             sksc7(i) = wcli                              *fine prg cliente
256700020321     c* solo record validi
256800020321if  3c                   IF        acratb = *blanks  AND                        *NO annullati
256900020321     c                             acrtcr <> 'M'                                *NO infrequenti
257000020321     c* memorizza i dati
257100020321     c                   EVAL      ii = ii + 1                                  *indice
257200020321     c                   EVAL      SCro(ii) = acrcro                            *codice cliente
257300020321e   3c                   ENDIF
257400020321     c*
257500020321     c* lettura successiva cliente ritiro
257600020321     c                   READ      fnacr01l                               98
257700020321     c*
257800020321     c* memorizza il cliente ritiro letto (senza prg) in una variabile di lavoro
257900020321     c                   MOVEL     acrcro        wcli                           *cliente senza prg
258000020321e   2c                   ENDDO                                                  *rottura cliente
258100020321     C*
258200020321     c                   ADD       1             i                              *lettura cl.figlio
258300020321e   1c                   ENDDO                                                  *fine clienti figli
258400020321     C*
258500020321     C                   ENDSR
258600020321     c*--------------------------------------------------------------------------------------------*
258700020321     c* Carica i clienti figli legati al cliente unificante di entrata
258800020321     c*--------------------------------------------------------------------------------------------*
258900020321     c     CarKun        BEGSR
259000020321     c*
259100020321     c* reimposta variabili di lavoro
259200020321     c                   CLEAR                   sksc7                          clienti da elaborare
259300020321     c*
259400020321     c* reperisce il legame clienti figli -se c'è- sul file delle abilitazioni
259500020321     c                   EVAL      kssksu = InpO121Ksu                          *cliente unificante
259600020321     c                   EVAL      kssisv = 'TT'                                *tipo servizio
259700020321     c     keyvss        SETLL     tivss02l
259800020321     c     keyvss        READE     tivss02l                               99
259900020321do  1c                   DOW       NOT *in99
260000020321if  2c                   IF        vssvat =  *blanks                            *attivo
260100020321if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
260200020321     c                             datcor <= vssdsc
260300020321     c*
260400020321     c* controlla, se per il cliente unificante, ci sono clienti figli da elaborare
260500020321if  4c                   IF        vsstle <> *blanks                            *SI legami
260600020321     c                   CLEAR                   tibs10ds
260700020321     c                   EVAL      d10drf = datcor                              *data riferimento
260800020321     c                   EVAL      d10tle = vsstle                              *legame
260900020321     c                   EVAL      d10paf = 'F'                                 *cerca i figli
261000020321     c                   MOVEL     vssksu        n8
261100020321     c                   Z-ADD     n8            d10cod                         *cliente unificante
261200020321     c                   CALL      'TIBS10R'                                    *cerca figli
261300020321     c                   PARM                    tibs10ds
261400020321if  5c                   IF        d10err <> ' '                                *SI errore: no figli
261500020321     c                   MOVE      InpO121Ksu    sksc7(1)                       *cliente unificante
261600020321x   5c                   ELSE                                                   *NO errore: si figli
261700020321     c                   Z-ADD     sksc11        sksc7                          *cliente unif+figli
261800020321e   5c                   ENDIF
261900020321x   4c                   ELSE                                                   *NO legami
262000020321     c                   MOVE      InpO121Ksu    sksc7(1)                       *cliente unificante
262100020321e   4c                   ENDIF
262200020321     c*
262300020321     c* se trovato cliente abilitato valido, esce dal ciclo di lettura
262400020321     c                   LEAVE                                                  *uscita ciclo
262500020321e   3c                   ENDIF
262600020321e   2c                   ENDIF
262700020321     c     keyvss        READE     tivss02l                               99
262800020321e   1c                   ENDDO                                                  *fine cl.abilitati
262900020321     c*
263000020321     c                   ENDSR
263100020321     c*--------------------------------------------------------------------------------------------*
263200060605
263300000000** -MSG- Messaggi di uscita                                                        num  cod
263400020325/ Codice cliente destinatario errato                                                1    1
263500020325/ Ragione sociale ritiro errata                                                     2    2
263600020325/ Indirizzo ritiro errato                                                           3    3
263700020325/ CAP ritiro errato                                                                 4    4
263800020325/ Località ritiro errata                                                            5    5
263900020325/ Provincia ritiro errata                                                           6    6
264000020325/ Nazione ritiro errata                                                             7    7
264100020321/ CAP, località e provincia ritiro incongruenti fra loro                            8    8
264200020321/ Peso errato                                                                       9    9
264300020321/ Numero colli errato                                                               10   A
264400020321/ Data di ritiro errata                                                             11   B
264500020321/ L' orario attuale non consente di effettuare il ritiro in giornata                12   C
264600020321/ Ora  pronta merce errata                                                          13   D
264700020328/ Persona e telefono per il ritiro errati                                           14   E
264800020325/ Dati pagamento errati                                                             15   F
264900020325/ Volume errato                                                                     16   G
265000020325/ Numero bancali errato                                                             17   H
265100020325/ Immettere almeno una quantità fra peso, volume o bancali                          18   I
265200020325/ Numero bilici  errato                                                             19   L
265300020325/ Numero autotreni errato                                                           20   M
265400020325/ Numero motrici errato                                                             21   N
265500020325/ Prima   ora apertura errata                                                       22   O
265600020325/ Seconda ora apertura errata                                                       23   P
265700020325/ Errori di congruità fra gli orari di apertura indicati                            24   Q
265800020325/ Inserire se la consegna è da effettuarsi da uno o più destinatari                 25   R
265900020325/ Inserire il codice destinatario se la consegna è per un SOLO destinatario         26   S
266000020325/ Inserire i dati del destinatario (codice o nome)                                  27   T
266100020325/ Inserire il nome   destinatario se la consegna è per un SOLO destinatario         28   U
266200020326/ Ragione sociale destinatario errata                                               29   V
266300020326/ Indirizzo destinatario errato                                                     30   Z
266400020326/ CAP destinatario errato                                                           31   X
266500020326/ Località destinatario errata                                                      32   Y
266600020325/ Provincia destinatario errata                                                     33   J
266700020325/ Nazione destinatario errata                                                       34   K
266800020325/ Dati destinatario errati                                                          35   W
266900020325/ Scegliere la modalità di consegna                                                 36   !
267000020325/ Scegliere il network estero                                                       37   "
267100020328/ Inserire o il codice o il nominativo destinatario per esteso                      38   £
267200020325                                                                                    39   $
267300020327                                                                                    40   %
