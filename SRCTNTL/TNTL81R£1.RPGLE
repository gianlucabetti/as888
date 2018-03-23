000100091001----- *-------------------------------------------------------------
000200091001      * La stampa dei Traini è con OVERLAY con la Firma di PEDINI.
000300091001----- *-------------------------------------------------------------
000400091001      * il prtf va compilato con i seguenti parametri
000500091001      * CRTPRTF FILE(GAITRAFIL/TNTL81P)
000600091001      * DEVTYPE(*AFPDS)
000700091001      * TEXT('Allegato contratto traini.')
000800091001      * PAGESIZE(70 120 *ROWCOL)
000900091001      * CPI(15)
001000091001      * OVRFLW(63)
001100091001      * DUPLEX(*YES)
001200091001      * FRONTOVL(*LIBL/TNTL81FF 0 0)
001300091001      * BACKOVL(*LIBL/TNTL81BK 0 0 *NOCONSTANT)
001400091001      * FORMTYPE(ALLEGTRAIN)
001500091001      * USRDTA(ALLEGTRAIN)
001600091001----- *-------------------------------------------------------------
001700090511     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
001800981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
001900000000     H*--------------------------------------------------------------*
002000091001      ******
002100091001      * PER ARRIVARE A CREARE il PRTF bisogna fare precedentemente delle operazioni.
002200091001      *
002300091001      *  Sul PC con WORD si deve fare un documento (.DOC) di un foglio A4 con tutte le fincature
002400091001      * necessarie e l'immagine della Firma.
002500091001      *   I DOC word devono essere 2 in quanto c'è il Fronte e Retro (retro solo con la firma)
002600091001      * Occorre portare su AS i 2 DOC word importandoli nell'\IFS\QDLS\directory\ per gestirli
002700091001      * con i comandi di sistema che devono trovare questi oggetti nella QDLS\ (è importante).
002800091001      *
002900091001      *     ========
003000091001      *      SU PC:
003100091001      *     ========
003200091001      * 1)  Occorre creare su PC un puntamento alla directory QDLS dell'IFS .
003300091001      * Aprire l'<Esplora Risorse> di Windows andare nel menù <Strumenti> e selezionare <Connetti
003400091001      * Unità di rete>. Attribuendo una Lettera Identificativa (es."Z") x configurare un nuovo Disco
003500091001      * di rete, al quale attribuire il seguente percorso: <\\AS888\QDLS\directory\ e confermare
003600091001      * il tutto attivando anche l'opzione di connessione ad ogni avvio del PC (per non farla
003700091001      * manualmente ogni volta) quindi si conferma il tutto cliccando su FINE e da questo
003800091001      * momento il PC può interagire con l'AS su quella parte di disco CONDIVISA.
003900091001      * 2)  Occorre avere il driver AFP400 IBM di stampa sul PC quindi una volta installato si può
004000091001      * selezionare come STAMPANTE (virtuale).
004100091001      * 3)  Aprire il primo documento Word (Front) ed eseguirne la stampa con il driver AFP400
004200091001      * MODIFICANDO sulle PROPRIETA' della Stampa il <Tipo di Emissione come [Sovrapposizione]> .
004300091001      * La stampa produrrà un oggetto sulla cartella QDLS dell'IFS dell'AS400 che dovrà
004400091001      * avere un nome rigorosamente di 8+3 caratteri la cui estensione dovrà essere <OLY>
004500091001      * 4)  Rinominare l'oggetto dando l'estenzione OLY nel nostro caso TNTL81FF.OLY.
004600091001      * 5)  Ripetere gli stessi passi anche per la pagina di Retro fatta con Word.
004700091001      * ottenendo l'oggetto TNTL81BK.OLY su QDLS\directory\ (la directory è = Bartolini).
004800091001      *
004900091001      *     ===========
005000091001      *      SU AS400:
005100091001      *     ===========
005200091001      * Prima della creazione del PRTF finale su AS si devono eseguire 3 Comandi x entrambe
005300091001      * i 2 <.DOC> A4. (Facciamo l'esempio di avere nella QDLS l'oggetto FIRMA.OLY).
005400091001      * 5)  CRTPF FILE(GAITRAFIL/TNTL81F) RCDLEN(32766) TEXT('x provare la firma in OVERLAY')
005500091001      * LVLCHK(*NO) Si genera un File dove andare ad inserire le informazioni successive.
005600091001      * (Contenitore Vuoto dove andare ad inserire le informazioni dell'Oggetto di Overlay.)
005700091001      * 6)  CPYFRMPCD FROMFLR(Bartolini) TOFILE(GAITRAFIL/TNTL81F) FROMDOC(TNTL81FF.OLY)
005800091001      * TRNTBL(*NONE)  Con questo comando si va a copiare il Documento di Word dentro il PF.
005900091001      * 7)  CRTOVL OVL(GAITRAFIL/TNTL81FF) FILE(GAITRAFIL/TNTL81F) MBR(TNTL81F)
006000091001      * TEXT('merge x stampare la Firma su PRTF interno')
006100091001      * Con questo comando si crea l'OGGETTO OVERLAY [*OVL] su AS400
006200091001      * -
006300091001      * ripetere i passi 5-6-7 per la seconda pagina (retro) <FIRMA2> (nome: TNTL81BK)
006400091001      * -
006500091001      * 8) in ultimo si crea il PRTF finale con gli Overlay di Fronte e Retro:
006600091001      * CRTPRTF FILE(GAITRAFIL/TNTL81P) DEVTYPE(*AFPDS) DUPLEX(*YES)
006700091001      * FRONTOVL(GAITRAFIL/TNTL81FF) BACKOVL(GAITRAFIL/TNTL81BK)
006800091001      * 9) ora il PRTF è pronto per essere OVERRIDEato
006900091001      * OVRPRTF FILE(QSYSPRT) TOFILE(GAITRAFIL/TNTL81P)
007000091001      ******
007100091001     H*--------------------------------------------------------------*
007200060320     Ftitll02l  if   e           k disk
007300110504     Ftntlt01l  if   e           k disk
007400060320     Ftntln01l  if   e           k disk
007500060320     Fanfrn01l  if   e           k disk
007600080918     Fancdp01l  if   e           k disk
007700060321     Fansog01l  if   e           k disk
007800080916     Fanind01l  if   e           k disk
007900060320     Fazorg01l  if   e           k disk
008000110504     F****ansif01l  if   e           k disk
008100090511     Ftntl81p   O    F  120        PRINTER prtctl(prtds) usropn
008200011026
008300020304     dkpjba          e ds
008400020304
008500020304     dparam            ds
008600060320     D  datae                         8s 0
008700060321     D  traz1                         8  0
008800060320     D  socie                         3
008900090512     D  dataCerta                     8s 0
009000090512
009100020402     d modo            s              8
009200020114     d base            s              1
009300020412     d cost3b          s            120
009400020412     d cost3c          s            120
009500080916     d cost3D          s            120
009600080916     d cost3E          s            120
009700090417     d cost3f          s            120
009800090417     d cost3g          s            120
009900090417     d cost3h          s            120
010000090417     d cost3i          s            120
010100060421     d costt           s             27
010200060321     d costa           s             74
010300060321     d costa2          s             35
010400060321     d costa2b         s             16
010500020411     d costa3          s            120
010600020411     d costa4          s            120
010700020411     d costb           s            120
010800020411     d cost1           s            120
010900020411     d cost2           s            120
011000020306     d cost3           s            120
011100020306     d cost4           s            120
011200020306     d cost5           s            120
011300020411     d cost5b          s            120
011400060526     d cost5c          s            120
011500060526     d cost5d          s            120
011600020306     d cost6           s            120
011700020411     d cost6a          s            120
011800090417     d cost6b          s            120
011900020411     d costi           s            120
012000020411     d costia          s            120
012100020411     d costib          s            120
012200020411     d costic          s            120
012300020411     d costid          s            120
012400020411     d costie          s            120
012500020423     d costif          s            120
012600080916     d costiH          s            120
012700080916     d costiI          s            120
012800020306     d cost7           s            120
012900070302     d cost7a          s            120
013000020411     d cost7b          s            120
013100020412     d cost7c          s            120
013200020412     d cost7d          s            120
013300020306     d cost8           s            120
013400020402     d cost8b          s            120
013500020402     d cost8c          s            120
013600020402     d cost8f          s            120
013700090417     d cost8g          s            120
013800090417     d cost8h          s            120
013900060510     d cost9           s             35
014000060322     d cost10          s             60
014100060322     d cost11          s             60
014200020313     d cost13          s             70
014300020305     d dataiso         s               d   datfmt(*iso)
014400020305     d dataeur         s               d   datfmt(*eur)
014500090511     d copiaVettore...
014600090511     d                 s               n
014700090512     D certaData       s              8s 0
014800020117     d prtds           ds
014900020117     d   spab                         3  0
015000020117     d   spaa                         3  0
015100020117     d   skab                         3  0
015200020117     d   skaa                         3  0
015300020117     d   line                         3  0
015400020308     D CMD             S             80    DIM(2) CTDATA PERRCD(1)              OVRPRTF
015500080916     d locemi          s             40
015600090930     d saltato         s              1    INZ(' ')
015700080916     D DVCOBADS      E DS
015800080916     D                                     INZ
015900080916     D CBAIBANDS     E DS
016000090114     d fnlvt1ds      e ds
016100090114     d ficn05ds      e ds
016200080916     ddgpi           e ds
016300110504     ***************************************************************************
016400110504     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
016500110504     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
016600110504     ***************************************************************************
016700110504     **
016800110504     ** Definizione strutture dati.
016900110504     **
017000110504      **************************************************************************
017100110504     D tibsSocI0...
017200110504     D               E DS                  QUALIFIED
017300110504     D                                     INZ
017400110504     D tibsSocO0...
017500110504     D               E DS                  QUALIFIED
017600110504     D                                     INZ
017700110504     ***************************************************************************
017800110504     **
017900110504     ** Definizione variabili modulo/programma.
018000110504     **
018100110504     ***************************************************************************
018200110504     D prmRqsOpCode...
018300110504     D                 S             10A
018400110504     D prmRpyOpCode...
018500110504     D                 S             10A
018600110504     D prmRpyIdMsg...
018700110504     D                 S             10I 0
018800110504     D prmRqsFormato...
018900110504     D                 S             10A
019000110504     D prmRqsDataSize...
019100110504     D                 S             10I 0
019200110504     D prmRpyFormato...
019300110504     D                 S             10A
019400110504     D prmRpyDataSize...
019500110504     D                 S             10I 0
019600110504
019700090511
019800090511     ***************************************************************************
019900090511     **
020000090511     ** Main.
020100090511     **
020200090511     ***************************************************************************
020300090511
020400090511     ** Stampo la copia per il vettore.
020500090511     c                   eval      copiaVettore = *on
020600090511     c                   exsr      esecuzione
020700090511
020800090511     ** Stampo la copia per il mittente.
020900090512     C*                  MOVEL(p)  CMD(1)        COMMAN           80
021000090512     C*                  Z-ADD     80            LUNG             15 5
021100090512     C*                  CALL      'QCMDEXC'
021200090512     C*                  PARM                    COMMAN
021300090512     C*                  PARM                    LUNG
021400090512     C*
021500090512     c*                  eval      copiaVettore = *off
021600090512     c*                  exsr      esecuzione
021700090512     C*
021800090512     C*                  MOVEL(p)  CMD(2)        COMMAN           80
021900090512     C*                  Z-ADD     80            LUNG             15 5
022000090512     C*                  CALL      'QCMDEXC'
022100090512     C*                  PARM                    COMMAN
022200090512     C*                  PARM                    LUNG
022300090511
022400090511     c                   seton                                        lr
022500090511
022600090511     ***************************************************************************
022700090511     **
022800090511     **
022900090511     **
023000090511     ***************************************************************************
023100090511     c     esecuzione    BEGSR
023200090511     c
023300090511     c                   reset                   prima
023400060321     c                   move      traz1         kpdr              7 0
023500020308     c
023600060321     c     kpdr          setll     titll02l
023700020308     c                   do        *hival
023800060321     c                   read      titll02l
023900060320      *fine file
024000060321     c                   if        %eof(titll02l)
024100060320     c                   leave
024200060320     c                   endif
024300060320      *trazionista singolo
024400060320     c                   if        kpdr > 0 and kpdr <> tllpdr
024500060320     c                   leave
024600060320     c                   endif
024700060324      *trazione a costo fittizio 1 euro da non inserire in elenco
024800060324     c                   if        tllili = 1
024900060324     c                   iter
025000060324     c                   endif
025100060321      *trazionista generico 9999999 valido solo se richiesto singolarmente
025200060321     c                   if        kpdr <> 9999999 and tllpdr = 9999999
025300060321     c                   leave
025400060321     c                   endif
025500060320      *leggo traino
025600060320     c     ktlt          chain     tntlt01l
025700060320     c                   if        %found(tntlt01l)
025800060321     c                   if        datae  >= tltdde and
025900060321     c                             datae  <= tltdsc
026000060321     c                   if        primo_giro = *blank
026100060321     c                   move      tllpdr        expdr
026200060321     c                   move      'X'           primo_giro        1
026300060321     c                   endif
026400060320     c                   exsr      elabora
026500060320     c                   else
026600060320     c                   iter
026700060320     c                   endif
026800020308
026900060320     c                   else
027000060320     c                   iter
027100060320     c                   endif
027200060320      *
027300020308     c                   enddo
027400090511
027500060321     c                   if        prima <> *BLANK
027600090511     C                   except    piede2
027700090511     C                   exsr      saltoPaginaF
027800090511     C                   exsr      saltoPaginaI
027900090511     C                   except    piede4
028000090511     C                   exsr      saltoPaginaF
028100090511     C                   exsr      saltoPaginaI
028200090511     C                   except    piede6
028300090511     C                   exsr      saltoPaginaF
028400090511     C                   exsr      saltoPaginaI
028500090511     C                   except    piede8
028600090511     C                   exsr      saltoPaginaF
028700090511     C                   exsr      saltoPaginaI
028800090511     C                   except    piede10
028900090511     C                   exsr      saltoPaginaF
029000090511     C                   exsr      saltoPaginaI
029100090511     C                   except    piede12
029200090511     C                   exsr      saltoPaginaF
029300090511     C                   exsr      saltoPaginaI
029400090511     C                   except    piede14
029500090511     C                   exsr      saltoPaginaF
029600090511     C                   exsr      saltoPaginaI
029700090511     C                   except    piede16
029800090511     C                   exsr      saltoPaginaF
029900090511     C                   exsr      saltoPaginaI
030000090511     C                   except    piede18
030100090511     C                   exsr      saltoPaginaF
030200090511     C                   exsr      saltoPaginaI
030300090511     C                   except    piede20
030400090511     C                   exsr      saltoPaginaF
030500090511     C                   exsr      saltoPaginaI
030600090511     C                   except    piede22
030700090511     C                   exsr      saltoPaginaF
030800090511     C                   exsr      saltoPaginaI
030900090511     C                   except    piede24
031000090511     C                   exsr      saltoPaginaF
031100090511     C                   exsr      saltoPaginaI
031200090511     C                   except    piede26
031300090511     C                   exsr      saltoPaginaF
031400090511     C                   exsr      saltoPaginaI
031500090511     C                   except    piede28
031600090511     C                   exsr      saltoPaginaF
031700090511     C                   exsr      saltoPaginaI
031800090511     C                   except    piede30
031900090511     C                   exsr      saltoPaginaF
032000090511     C                   exsr      saltoPaginaI
032100090511     C                   except    piede32
032200090511     C                   exsr      saltoPaginaF
032300090511     C                   exsr      saltoPaginaI
032400090511     C                   except    piede34
032500090511     C                   exsr      saltoPaginaF
032600090511     C                   exsr      saltoPaginaI
032700090511     C                   except    piede36
032800090511     C                   exsr      saltoPaginaF
032900090511     C                   exsr      saltoPaginaI
033000090511     C                   except    piede38
033100090511     C                   exsr      saltoPaginaF
033200090511     C                   exsr      saltoPaginaI
033300090511     C                   except    piede40
033400090511     C                   exsr      saltoPaginaF
033500090511     C                   exsr      saltoPaginaI
033600090511     C                   except    piede42
033700090511     C                   exsr      saltoPaginaF
033800090511     C                   exsr      saltoPaginaI
033900090511     C                   except    piede44
034000090511     C                   exsr      saltoPaginaF
034100090511     C                   exsr      saltoPaginaI
034200090511     C                   except    piede46
034300090511     C                   exsr      saltoPaginaF
034400090511     C                   exsr      saltoPaginaI
034500090511     C                   except    piede48
034600090511     C                   exsr      saltoPaginaF
034700090511     C                   exsr      saltoPaginaI
034800090511     C                   except    piede50
034900090511     C                   exsr      saltoPaginaF
035000090511     C                   exsr      saltoPaginaI
035100090511     C                   except    piede52
035200090511     C                   exsr      saltoPaginaF
035300090511     C                   exsr      saltoPaginaI
035400090511     C                   except    piede54
035500090511     C                   exsr      saltoPaginaF
035600090511     C                   exsr      saltoPaginaI
035700090511     C                   except    piede56
035800090511     C                   exsr      saltoPaginaF
035900090511     C                   exsr      saltoPaginaI
036000090511     C                   except    piede58
036100090511     C                   exsr      saltoPaginaF
036200090511     C                   exsr      saltoPaginaI
036300090511     C                   except    piede60
036400090511     C                   exsr      saltoPaginaF
036500090511     C                   exsr      saltoPaginaI
036600090511     C                   except    piede62
036700090511     C                   exsr      saltoPaginaF
036800090511     C                   exsr      saltoPaginaI
036900090511     c                   except    piede70
037000090511     C                   exsr      saltoPaginaF
037100090511     C                   exsr      saltoPaginaI
037200090511     c                   except    piede72
037300090511     C                   exsr      saltoPaginaF
037400090511     C                   exsr      saltoPaginaI
037500090511     c                   except    piede74
037600090511     C                   exsr      saltoPaginaF
037700090511     C                   exsr      saltoPaginaI
037800090511     c                   except    piede80
037900090511     C                   exsr      saltoPaginaF
038000090511     C                   exsr      saltoPaginaI
038100090511     c                   except    piede82
038200090511     C                   exsr      saltoPaginaF
038300090511     C                   exsr      saltoPaginaI
038400090511     c                   except    piede84
038500090511     C                   exsr      saltoPaginaF
038600090511     C                   exsr      saltoPaginaI
038700090511     c                   except    piede86
038800090511     C                   exsr      saltoPaginaF
038900090511     C                   exsr      saltoPaginaI
039000090511     c                   except    piede88
039100090930      *
039200090511     c                   if        copiaVettore
039300090511     c                   except    firmaMitt
039400090511     c                   ENDIF
039500090930      *
039600090930     c                   if        saltato = *blank
039700090930     c                   except    PagBIANCA
039800090930     c                   end
039900090930      *
040000090511     C                   close     tntl81p
040100090511     c                   endif
040200090511
040300090511     c                   ENDSR
040400090511
040500020308      *--------------------------------------------------
040600020308     c     elabora       begsr
040700020308      *--------------------------------------------------
040800020411     c
040900060320     c                   if        tllpdr <> expdr and prima <> *blank
041000090511     C                   except    piede2
041100090511     C                   exsr      saltoPaginaF
041200090511     C                   exsr      saltoPaginaI
041300090511     C                   except    piede4
041400090511     C                   exsr      saltoPaginaF
041500090511     C                   exsr      saltoPaginaI
041600090511     C                   except    piede6
041700090511     C                   exsr      saltoPaginaF
041800090511     C                   exsr      saltoPaginaI
041900090511     C                   except    piede8
042000090511     C                   exsr      saltoPaginaF
042100090511     C                   exsr      saltoPaginaI
042200090511     C                   except    piede10
042300090511     C                   exsr      saltoPaginaF
042400090511     C                   exsr      saltoPaginaI
042500090511     C                   except    piede12
042600090511     C                   exsr      saltoPaginaF
042700090511     C                   exsr      saltoPaginaI
042800090511     C                   except    piede14
042900090511     C                   exsr      saltoPaginaF
043000090511     C                   exsr      saltoPaginaI
043100090511     C                   except    piede16
043200090511     C                   exsr      saltoPaginaF
043300090511     C                   exsr      saltoPaginaI
043400090511     C                   except    piede18
043500090511     C                   exsr      saltoPaginaF
043600090511     C                   exsr      saltoPaginaI
043700090511     C                   except    piede20
043800090511     C                   exsr      saltoPaginaF
043900090511     C                   exsr      saltoPaginaI
044000090511     C                   except    piede22
044100090511     C                   exsr      saltoPaginaF
044200090511     C                   exsr      saltoPaginaI
044300090511     C                   except    piede24
044400090511     C                   exsr      saltoPaginaF
044500090511     C                   exsr      saltoPaginaI
044600090511     C                   except    piede26
044700090511     C                   exsr      saltoPaginaF
044800090511     C                   exsr      saltoPaginaI
044900090511     C                   except    piede28
045000090511     C                   exsr      saltoPaginaF
045100090511     C                   exsr      saltoPaginaI
045200090511     C                   except    piede30
045300090511     C                   exsr      saltoPaginaF
045400090511     C                   exsr      saltoPaginaI
045500090511     C                   except    piede32
045600090511     C                   exsr      saltoPaginaF
045700090511     C                   exsr      saltoPaginaI
045800090511     C                   except    piede34
045900090511     C                   exsr      saltoPaginaF
046000090511     C                   exsr      saltoPaginaI
046100090511     C                   except    piede36
046200090511     C                   exsr      saltoPaginaF
046300090511     C                   exsr      saltoPaginaI
046400090511     C                   except    piede38
046500090511     C                   exsr      saltoPaginaF
046600090511     C                   exsr      saltoPaginaI
046700090511     C                   except    piede40
046800090511     C                   exsr      saltoPaginaF
046900090511     C                   exsr      saltoPaginaI
047000090511     C                   except    piede42
047100090511     C                   exsr      saltoPaginaF
047200090511     C                   exsr      saltoPaginaI
047300090511     C                   except    piede44
047400090511     C                   exsr      saltoPaginaF
047500090511     C                   exsr      saltoPaginaI
047600090511     C                   except    piede46
047700090511     C                   exsr      saltoPaginaF
047800090511     C                   exsr      saltoPaginaI
047900090511     C                   except    piede48
048000090511     C                   exsr      saltoPaginaF
048100090511     C                   exsr      saltoPaginaI
048200090511     C                   except    piede50
048300090511     C                   exsr      saltoPaginaF
048400090511     C                   exsr      saltoPaginaI
048500090511     C                   except    piede52
048600090511     C                   exsr      saltoPaginaF
048700090511     C                   exsr      saltoPaginaI
048800090511     C                   except    piede54
048900090511     C                   exsr      saltoPaginaF
049000090511     C                   exsr      saltoPaginaI
049100090511     C                   except    piede56
049200090511     C                   exsr      saltoPaginaF
049300090511     C                   exsr      saltoPaginaI
049400090511     C                   except    piede58
049500090511     C                   exsr      saltoPaginaF
049600090511     C                   exsr      saltoPaginaI
049700090511     C                   except    piede60
049800090511     C                   exsr      saltoPaginaF
049900090511     C                   exsr      saltoPaginaI
050000090511     C                   except    piede62
050100090511     C                   exsr      saltoPaginaF
050200090511     C                   exsr      saltoPaginaI
050300090511     c                   except    piede70
050400090511     C                   exsr      saltoPaginaF
050500090511     C                   exsr      saltoPaginaI
050600090511     c                   except    piede72
050700090511     C                   exsr      saltoPaginaF
050800090511     C                   exsr      saltoPaginaI
050900090511     c                   except    piede74
051000090511     C                   exsr      saltoPaginaF
051100090511     C                   exsr      saltoPaginaI
051200090511     c                   except    piede80
051300090511     C                   exsr      saltoPaginaF
051400090511     C                   exsr      saltoPaginaI
051500090511     c                   except    piede82
051600090511     C                   exsr      saltoPaginaF
051700090511     C                   exsr      saltoPaginaI
051800090511     c                   except    piede84
051900090511     C                   exsr      saltoPaginaF
052000090511     C                   exsr      saltoPaginaI
052100090511     c                   except    piede86
052200090511     C                   exsr      saltoPaginaF
052300090511     C                   exsr      saltoPaginaI
052400090511     c                   except    piede88
052500090930      *
052600090511     c                   if        copiaVettore
052700090511     c                   except    firmaMitt
052800090511     c                   ENDIF
052900060321     c                   move      tllpdr        expdr             7 0
053000060321     c                   clear                   prima             1
053100090511     C                   close     tntl81p
053200060320     c                   endif
053300060320
053400060320     c                   if        prima = *blank
053500090511     C                   OPEN      tntl81p
053600060321     c                   z-add     0             page
053700060321     c                   move      expdr         fornit
053800080916     c* imposto costanti nel caso non reperisco anagrafico
053900080916     c                   eval      costia= 'Ragione sociale ___________________+
054000080916     c                             ____________________________________________'
054100080916
054200080916     c                   eval      costib= 'Indirizzo__________________________+
054300080916     c                             ____________________________________________'
054400080916
054500080916     c                   eval      costic= 'CAP______________ Città____________+
054600080916     c                             ____________________________________________'
054700080916
054800080916     c                   eval      costif= 'Recapito __________________________+
054900080916     c                             ____________________________________________'
055000080916     c                   eval      costid= 'Partita IVA _______________________+
055100080916     c                             ____________________________________________'
055200080916     c                   eval      cost8c= '|__|__|__|__|  -
055300080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
055400080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
055500060321      * decodifico fornitore
055600060321     C                   clear                   ragsvet
055700080918     c                   movel     *all'_'       cdpdescr
055800060321     C     kfrn          chain     anfrn01l
055900060321     c                   if        %found(anfrn01l)
056000080918     C     kcdp          chain     ancdp01l
056100060321     C     frnsogg       chain     ansog01l
056200060321     c                   if        %found(ansog01l)
056300080919     C                   MOVEL     sogdes        ragsvet          44
056400060321     c                   endif
056500060321     c                   endif
056600080918     c                   eval      cost8b= %trimr(cdpdescr) +' '+
056700080918     c                             'disposto su codice IBAN:'
056800080916     c* indirizzo
056900080916     c                   clear                   tipo
057000080916     c                   clear                   codice
057100080916     C     Kind          CHAIN     anind01l
057200080916     c                   if        %found(anind01l)
057300080916     C                   eval      costia = 'Ragione sociale ' + sogdes
057400080916     C                   eval      costib = 'Indirizzo ' + indindriz
057500080916     c                   if        indprov <> ' '
057600080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
057700080916     c                   else
057800080916     C                   eval      locemi = indlocalit
057900080916     c                   end
058000080916     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
058100080916     c                             locemi
058200080916     c                   end
058300080916     C                   eval      costid = 'Partita IVA ' + sogpartiva
058400091218      **
058500091218      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
058600091218      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
058700091218      **
058800091218      **  Aggiunge il Codice Fiscale
058900091218     c                   if        sogcdfisc <> ' ' and indprov <> ' '
059000091218     c                   eval      costiD = %Trim(costiD) +
059100091218     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
059200091218     C                             ': ' + %trimr(sogcdfisc)
059300091218     c                   end
059400091218      **
059500091218     c                   if        indtelex <> ' ' and
059600091218     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
059700091218     c                   eval      costiD = %Trim(costiD) + ' ' + indtelex
059800091218     c
059900091218     c                   end
060000091218      **
060100080916     c* indirizzo recapito
060200080916     C                   SETOFF                                       89
060300080916     c                   eval      tipo ='IF'
060400080916     c                   eval      codice ='000'
060500080916     C     Kind          CHAIN     anind01l
060600080916     c                   if        %found(anind01l)
060700080916     C                   SETON                                        89
060800080916     c                   eval      costif= 'Recapito ' + sogdes
060900080916     C                   eval      costiH = indindriz
061000080916     c                   if        indprov <> ' '
061100080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
061200080916     c                   else
061300080916     C                   eval      locemi = indlocalit
061400080916     c                   end
061500080916     C                   eval      costiI = indcap + ' ' +
061600080916     c                             locemi
061700080916     c                   end
061800080916     c* reperisco IBAN
061900080916     C                   RESET                   DVCOBADS
062000080916     C                   RESET                   CBAIBANDS
062100080916
062200080916     C                   EVAL      CoBSoc = socie
062300080916     C                   EVAL      CoBCOF = 'F'
062400080916     C                   EVAL      CoBKsc = fornit
062500080916     C                   CALL      'DVCOBA'
062600080916     C                   PARM                    DVCOBADS
062700080916A0846C                   PARM                    CBAIBANDS
062800080916     C                   IF        CoBErr = *OFF
062900080916     C                   EVAL      cost8c = CBaStato + CbaCheck
063000080916     C                             + %TRIMR(CBaCoorI)
063100080916     c                   end
063200110504      *
063300110504     C*** SPOSTATA QUI LA DECODIFICA DELLA RAGIONE SOCIALE MITTENTE
063400110504     c                   exsr      decod_societa
063500110504      *
063600110504      *
063700060321      *stampa testata allegato
063800020306     c                   except    testata
063900090512     c                   except    tesDtCerta
064000060320     c                   except    testo
064100060320     c                   move      'X'           prima
064200020306     c                   endif
064300090511     c*                  if        line > 60
064400090511     c*                  except    testata
064500090511     c*                  endif
064600090511     C                   exsr      saltoPaginaF
064700090511     C                   exsr      saltoPaginaI
064800060320      *decodifica partenza
064900060320     c     tlttfp        chain     azorg01l
065000060320     c                   if        %found(azorg01l)
065100060320     c                   movel     orgdes        partenza         20
065200060320     c                   else
065300060320     c                   clear                   partenza
065400060320     c                   endif
065500060320      *decodifica arrivo
065600060320     c     tlttfa        chain     azorg01l
065700060320     c                   if        %found(azorg01l)
065800060321     c                   movel     orgdes        arrivo           20
065900060320     c                   else
066000060321     c                   clear                   arrivo
066100060320     c                   endif
066200090114      * reperimento kilometri tappe traino
066300090402     c*mm                clear                   fnlvt1ds
066400090402     c*mm                eval      it1trn = tlltrn
066500090402     c*mm                eval      it1dde = datae
066600090402     c*mm                call      'FNLVT1R'
066700090402     C*mm                parm                    fnlvt1ds
066800090402     c*mm                if        ot1err = *blank
066900090402     c*mm                eval      totkm  = ot1km
067000090402     c*mm                else
067100090402     c*mm                clear                   totkm             6 0
067200090402     c*mm                endif
067300060320      * stampa riga listino attiva
067400060320     c                   except    dettaglio
067500060320      *reperimento nota
067600060320     c     ktln          chain     tntln01l
067700060320     c                   if        %found(tntln01l)
067800060320     c                   except    dettanote
067900060320     c                   endif
068000060320
068100020308     c                   endsr
068200090511
068300090511     ***************************************************************************
068400090511     **
068500090511     ** Salto pagina: fine pagina.
068600090511     **
068700090511     ***************************************************************************
068800090511     C     saltoPaginaF  BEGSR
068900090511     c                   if        line > 60 AND copiaVettore
069000090511     c                   except    firmaMitt
069100090930     c                   eval      saltato = 'S'
069200090511     c                   ENDIF
069300090511     C                   ENDSR
069400090511
069500090511     ***************************************************************************
069600090511     **
069700090511     ** Salto pagina: inizio nuova pagina.
069800090511     **
069900090511     ***************************************************************************
070000090511     C     saltoPaginaI  BEGSR
070100090511     c                   if        line > 60
070200090511     c                   except    testata
070300090512     c                   if        %rem(page : 2) = 1
070400090930     c                   clear                   saltato
070500090512     c                   except    tesDtCerta
070600090512     c                   except    saltino
070700090511     c                   ENDIF
070800090511     c                   endif
070900090511     C                   ENDSR
071000020304
071100011026     c*-------------------------------------------------
071200011026     c     *inzsr        begsr
071300011026     c*-------------------------------------------------
071400020304     c     *entry        plist
071500020304     c                   parm                    kpjba
071600020304     c                   movel     kpjbu         param
071700020304
071800060320     c     kfrn          klist
071900060320     c                   kfld                    socie
072000060320     c                   kfld                    fornit            8
072100060320     c                   move      *all'0'       fornit
072200080918     c     kcdp          klist
072300080918     c                   kfld                    socie
072400080918     c                   kfld                    frncondpag
072500080916     c     kind          klist
072600080916     c                   kfld                    frnsogg
072700080916     c                   kfld                    tipo              2
072800080916     c                   kfld                    codice            3
072900021203
073000060320     c     ktlt          klist
073100060320     c                   kfld                    tlltrn
073200060320     c                   kfld                    tlldde
073300020402
073400060320     c     ktln          klist
073500060320     c                   kfld                    tlltrn
073600060320     c                   kfld                    tlldde
073700060320     c                   kfld                    numfe             3 0
073800060320     c                   kfld                    numpro            3 0
073900060322     c                   z-add     1             numpro
074000060320
074100020218     C                   time                    w0120            14 0
074200020218     C                   move      w0120         wdat              8 0
074300020308      * data da impostare su data stampa e convalida se lancio = 'C'
074400020308     c                   move      wdat          dataeur
074500020308     c                   move      dataeur       dataiso
074600020308     c                   move      dataiso       dataoggi          8 0
074700020308      * data da elaborare
074800060321     c                   move      datae         dataiso
074900020304     c                   move      dataiso       dataeur
075000020304     c                   move      dataeur       alladata          8 0
075100090512      * data certa
075200090512     c                   move      dataCerta     dataiso
075300090512     c                   move      dataiso       dataeur
075400090512     c                   move      dataeur       certaData
075500011026
075600020117     c                   z-add     99            line
075700020305     c                   move      *all'_'       separa          100
075800060321      * ragione sociale società mittente
075900110504      *****
076000110504     c**** socie         chain     ansif01l
076100110504     c****               if        %found(ansif01l)
076200110504     c****               movel     sifrgs        ragsmit          30
076300110504     c****               endif
076400110504      *****
076500110504      *****  Decodifica ragione sociale mittente SPOSTATA PRIMA DELLA EXCPT
076600110504      *****
076700080916     c* prezzo iniziale carburante legge 133
076800090114     c                   clear                   ficn05ds
076900090114     c                   call      'FICN05R'
077000090114     c                   parm                    ficn05ds
077100090114     c                   eval      dgpi = o05gpi
077200090114     c                   z-add     D§GPIGPI      prezzo            5 3
077300090114
077400090417     c                   eval      costt = 'ALLEGATO  B   IN VIGORE DAL'
077500020412     c
077600080916     c* imposto costanti nel caso non reperisco anagrafico
077700080916     c                   eval      costia= 'Ragione sociale ___________________+
077800080916     c                             ____________________________________________'
077900080916
078000080916     c                   eval      costib= 'Indirizzo__________________________+
078100080916     c                             ____________________________________________'
078200080916
078300080916     c                   eval      costic= 'CAP______________ Città____________+
078400080916     c                             ____________________________________________'
078500080916
078600080916     c                   eval      costif= 'Recapito __________________________+
078700080916     c                             ____________________________________________'
078800080916     c                   eval      costid= 'Partita IVA _______________________+
078900080916     c                             ____________________________________________'
079000080916     c                   eval      cost8c= '|__|__|__|__|  -
079100080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
079200080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
079300020411     c*costanti pretariffa
079400020411     c                   eval      costa = 'Allegato tariffario al contratto +
079500060321     c                             di trasporto sottoscritto tra la società '
079600060321     c                   eval      costa2= '(di seguito denominata MITTENTE) e '
079700060321     c                   eval      costa2b= 'codice fornitore'
079800060321     c                   eval      costa3= '(di seguito denominata VETTORE)'
079900060321     c
080000060321     c                   eval      cost1 = 'Le parti come convenuto all''art.+
080100060321     c                             10 del suddetto contratto di trasporto, con+
080200020411     c                             cordano i corrispettivi per i servizi di'
080300020423     c                   eval      cost2 = 'trasporto, comprensivi delle eventu-
080400060321     c                             ali operazioni accessorie, nonchè di quals+
080500060321     c                             iasi onere necessario per l''esecuzione '
080600060321     c                   eval      costb = 'del trasporto (pedaggi, diritti di +
080700060321     c                             transito, equipaggiamento del veicolo, ecc.)+
080800060321     c                              secondo la seguente tariffa :'
080900020306
081000020411     c*costanti piede
081100020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
081200020411     c                             he i corrispettivi indicati nelle sopraccita+
081300020411     c                             te tariffe possano essere oggetto di'
081400020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
081500020411     c                             one, a seguito di modifiche pattuite tra le +
081600020412     c                             parti del progetto operativo'
081700020412     c                   eval      cost3c= 'originariamente concordato.'
081800080916     c                   eval      cost3d= 'Il vettore dichiara espressamente -
081900080916     c                             che i corrispettivi di cui al presente alleg-
082000080916     c                             ato sono stati concordati'
082100080916     c                   eval      cost3e= 'con il mittente sulla base del -
082200080916     c                             costo del carburante pari a euro xx,xxx/lt.'
082300090417     c                   eval      cost3f= 'Ai sensi e per gli effetti dell''ar+
082400090417     c                             t. 6, comma 3 lett. e-bis D. Lgs. 286/05, +
082500090417     c                             il contratto di trasporto '
082600090417     c                   eval      cost3g= 'di cui il presente costituisce alle+
082700090417     c                             gato, deve intendersi integrato con l''indi+
082800090417     c                             cazione dei tempi massimi '
082900090417     c                   eval      cost3h= 'di carico e scarico che le stesse +
083000090417     c                             parti indicano in complessive ore 2 (due) +
083100090417     c                             in relazione a ciascun'
083200090417     c                   eval      cost3i= 'servizio di trasporto da effettuars+
083300090417     c                             i nell''arco della giornata.'
083400080916
083500020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
083600020402     c                              società da questi delegata ad emetter+
083700020411     c                              e alla fine di ogni mese in nome e per '
083800020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
083900020411     c                             a relativa ai servizi di trasporto ed alle o+
084000020423     c                             perazioni accessorie effettuate'
084100060526     c                   eval      cost5b= 'in tale periodo. Tale fattura com-
084200060526     c                             prenderà anche i corrispettivi per le presta+
084300060526     c                             zioni di trasporto non previste'
084400020306
084500090417     c                   eval      cost5c= 'dal presente allegato B, alle quali-
084600060526     c                              le parti in ogni caso riconoscono la piena +
084700060526     c                             validità ed efficacia del '
084800060526
084900060526     c                   eval      cost5d= 'contenuto degli articoli 10 e 14 -
085000060526     c                             del contratto di trasporto.'
085100060526
085200020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
085300060321     c                             dei fogli viaggio  che contengono in dettagl+
085400060321     c                             io i servizi di trasporto e le '
085500020411
085600060321     c                   eval      cost6a= 'operazioni accessorie effettuate.'
085700090417     c                   eval      cost6b= 'La fattura sarà emessa con IVA +
085800090417     c                             ad esigibilità immediata.'
085900020411
086000020411     c                   eval      costi = 'L''intestazione del vettore da rip+
086100020411     c                             ortare sulla fattura emessa è:'
086200020411
086300020411     c                   eval      costie= 'Il vettore in caso di variazione de+
086400020412     c                             ll''intestazione ne darà immediata comunic+
086500020412     c                             azione al mittente.'
086600020411
086700020423     c                   eval      cost7 = 'Il presente allegato decorre d+
086800070302     c                             alla data del __________,annulla e sostituis+
086900070302     c                             ce qualsiasi precedente patto verbale o'
087000070302     c                   eval      cost7a= 'scritto relativo al corrispettivo +
087100070302     c                             per l''esecuzione di servizi di trasporto +
087200070302     c                             e costituisce parte integrante del '
087300070302     c                   eval      cost7b= 'contratto di trasporto suddetto.'
087400020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
087500020412     c                             la piena validità ed efficacia del contenut+
087600060321     c                             o degli articoli 10 e 14 del'
087700020412     c                   eval      cost7d='contratto di trasporto.'
087800020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
087900020411     c                             pagamento dei corrispettivi fatturati verrà +
088000080918     c                             effettuato dalla mittente mediante'
088100020402
088200020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
088300020411     c                             lle coordinate bancarie ne darà +
088400020412     c                             immediata comunicazione al mittente.'
088500090417     c                   eval      cost8g='Il vettore approva espressamente il +
088600090417     c                             contenuto del presente allegato ed in partic+
088700090417     c                             olare le pattuizioni'
088800090417     c                   eval      cost8h= 'relative alla quantificazione, +
088900090417     c                             modalità e termini di pagamento dei +
089000090417     c                             corrispettivi.'
089100090417
089200060510     c                   eval      cost9 = 'Bologna li, _______________________'
089300020411     c                   eval      cost10= 'Firma del Vettore _________________+
089400060322     c                             _________________________'
089500090511     c*                  eval      cost11= 'Firma del Mittente_________________+
089600090511     c*                            _________________________'
089700020313     c                   eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
089800020313     c                              VALIDA CON I REFERIMENTI RICHIESTI'
089900020313
090000011026     c                   endsr
090100011129     C*--------------------------------------------------------------------
090200110504     C* decodifica la società
090300110504     C*--------------------------------------------------------------------
090400110504     C     Decod_societa begSR
090500110504      *
090600110504     c                   move      DataE         dataiso
090700110504     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
090800110504     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
090900110504     C                   RESET                   tibsSocI0
091000110504     C                   EVAL      tibsSocI0.idSocieta = socie
091100110504     C                   EVAL      tibsSocI0.DTVALIDITA = dataiso
091200110504     C
091300110504     C                   CALL      'TIBSSOCR'
091400110504     C                   PARM      'GETANAGRAF'  prmRqsOpCode
091500110504     C                   PARM      *BLANK        prmRpyOpCode
091600110504     C                   PARM      *ZERO         prmRpyIdMsg
091700110504     C                   PARM      'TIBSSOCI0'   prmRqsFormato
091800110504     C                   PARM                    tibsSocI0
091900110504     C                   PARM                    prmRqsDataSize
092000110504     C                   PARM      'TIBSSOCO0'   prmRpyFormato
092100110504     C                   PARM                    tibsSocO0
092200110504     C                   PARM                    prmRpyDataSize
092300110504      *
092400110504     c                   clear                   ragsmit          30
092500110504     c                   if         PRMRPYIDMSG >= 0
092600110504     c                              and TIBSSOCO0.IDSOCIETA <> *blank
092700110504     c                   eval       ragsmit = tibsSocO0.RAGSOCIALE
092800110504     c                   end
092900110504      *
093000110504     C                   ENDSR
093100110504     C************************************************************
093200090511
093300090511     Otntl81p   E            testata          02
093400060421     o                       costt               27
093500090512     o                       alladata          +  2 '0  .  .    '
093600090930     o*******                                   108 'Pag.'
093700090930     O                       page          z    115
093800090511     O*         E            testata     0
093900090511     o*                      costt               27
094000090511     o*                      alladata          +  2 '  /  /    '
094100090511     O*         E            testata     0
094200090511     o*                      costt               27
094300090511     o*                      alladata          +  2 '  /  /    '
094400090512     O          E            tesDtCerta       07
094500090512     O                       certaData           47 '0  .  .    '
094600020411      *testo previsto per collegamento al contratto
094700090511     O          E            testo            11
094800060321     o                       costa               74
094900060321     o                       ragsmit            110
095000020411     O          E            testo       1
095100060321     o                       costa2              35
095200060321     o                       ragsvet             80
095300060321     o                       costa2b            100
095400060321     o                       frnksc             110
095500020411     O          E            testo       1
095600020411     o                       costa3             120
095700020306     O          E            testo       1
095800020411     o                       cost1              120
095900020306     O          E            testo       1
096000020411     o                       cost2              120
096100020306      *
096200020402     O          E            testo       1
096300020411     o                       costb              120
096400020402      *
096500060320     O          E            testo       1
096600020305     o                       separa             100
096700060320     O          E            testo       2
096800060320     o                                           10 'Nr.Traino'
096900060321     o                                           21 'Partenza'
097000060321     o                                           45 'Arrivo'
097100060321     o                                           77 'Importo'
097200090402     o*mm                                        87 'Km.'
097300090511     O*         E            testo       0
097400090511     o*                                          10 'Nr.Traino'
097500090511     o*                                          21 'Partenza'
097600090511     o*                                          45 'Arrivo'
097700090511     o*                                          77 'Importo'
097800090402     o*mm                                        87 'Km.'
097900090511     O*         E            testo       0
098000090511     o*                                          10 'Nr.Traino'
098100090511     o*                                          21 'Partenza'
098200090511     o*                                          45 'Arrivo'
098300090511     o*                                          77 'Importo'
098400090402     o*mm                                        87 'Km.'
098500060320      *dettaglio
098600060320     O          E            dettaglio   1
098700060320     o                       tlltrn        z      8
098800060322     o                       partenza          +  5
098900060322     o                       arrivo            +  6
099000060320     o                       tllili        2   +  2
099100090402     o*mm                    totkm         2     87
099200090511     O*         E            dettaglio   0
099300090511     o*                      tlltrn        z      8
099400090511     o*                      partenza          +  5
099500090511     o*                      arrivo            +  6
099600090511     o*                      tllili        2   +  2
099700090402     o*mm                    totkm         2     87
099800090511     O*         E            dettaglio   0
099900090511     o*                      tlltrn        z      8
100000090511     o*                      partenza          +  5
100100090511     o*                      arrivo            +  6
100200090511     o*                      tllili        2   +  2
100300090402     o*mm                    totkm         2     87
100400060320     O          E            dettanote   1
100500060324     o                                           10 'Percorso'
100600060322     o                       tlntno              73
100700020411      *testo previsto per collegamento al contratto piede
100800020411      * parte prima
100900090511     O          E            piede2      2
101000020306     o                       cost3              120
101100090511     O          E            piede4      1
101200020411     o                       cost3b             120
101300090511     O          E            piede6      1
101400020412     o                       cost3c             120
101500090511     O          E            piede8      1
101600080916     o                       cost3D             120
101700090511     O          E            piede10     1
101800080916     o                       cost3E             120
101900080916     o                       prezzo        2     70
102000090511     O          E            piede12     1
102100090417     o                       cost3f             120
102200090511     O          E            piede14     1
102300090417     o                       cost3g             120
102400090511     O          E            piede16     1
102500090417     o                       cost3h             120
102600090511     O          E            piede18     1
102700090417     o                       cost3i             120
102800090511     O          E            piede20     1
102900020306     o                       cost4              120
103000090511     O          E            piede22     1
103100020306     o                       cost5              120
103200090511     O          E            piede24     1
103300020411     o                       cost5b             120
103400090511     O          E            piede26     1
103500060526     o                       cost5c             120
103600090511     O          E            piede28     1
103700060526     o                       cost5d             120
103800020411      * parte seconda
103900090511     O          E            piede30     1
104000020306     o                       cost6              120
104100090511     O          E            piede32     1
104200020411     o                       cost6a             120
104300090511     O          E            piede34     1
104400090417     o                       cost6b             120
104500020411      * parte terza
104600090511     O          E            piede36     1
104700020411     o                       costi              120
104800090511     O          E            piede38     1
104900020411     o                       costia             120
105000090511     O          E            piede40     1
105100020411     o                       costib             120
105200090511     O          E            piede42     1
105300020411     o                       costic             120
105400090511     O          E            piede44     1
105500080916     o                       costid             120
105600090511     O          E            piede46     1
105700020423     o                       costif             120
105800090511     O          E    89      piede48     1
105900080916     o                       costiH             120
106000090511     O          E    89      piede50     1
106100080916     o                       costiI             120
106200090511     O          E            piede52     1
106300020411     o                       costie             120
106400020411      * parte quarta
106500090511     O          E            piede54     1
106600020306     o                       cost7              120
106700090512     o                       alladata            54 '0  .  .    '
106800090511     O          E            piede56     1
106900070302     o                       cost7a             120
107000090511     O          E            piede58     1
107100020411     o                       cost7b             120
107200090511     O          E            piede60     1
107300020412     o                       cost7c             120
107400090511     O          E            piede62     1
107500020412     o                       cost7d             120
107600020411      * parte quinta
107700090511     O          E            piede70     1
107800020306     o                       cost8              120
107900090511     O          E            piede72     1
108000020402     o                       cost8b             120
108100090511     O          E            piede74     1
108200020402     o                       cost8c             120
108300090511     O          E            piede80     1
108400020402     o                       cost8f             120
108500090511     O          E            piede82     1
108600090417     o                       cost8g             120
108700090511     O          E            piede84     1
108800090417     o                       cost8h             120
108900020411      * parte sesta
109000090511     O          E            piede86     2
109100060510     o                       cost9               35
109200090511     O          E            piede88     2
109300090511     o*                      cost11              60
109400060321     o                       cost10             120
109500090511     o*
109600090511     O          E            firmaMitt        66
109700090511     O                       ragsmit             85
109800090511     o*
109900090511     O          E            saltino          10
110000090930     o*
110100090930     O          E            PagBIANCA  05
110200090511     o                                            1 ' '
110300090930     o                                           +1 'PAGINA VOLUTAMENTE LASCIAT'
110400090930     o                                           +0 'A IN BIANCO'
110500090930     o
110600020313**
110700090511OVRPRTF FILE(TNTL81P) FRONTOVL(TNTL81FX) BACKOVL(*NONE)
110800090511DLTOVR FILE(TNTL81P)
