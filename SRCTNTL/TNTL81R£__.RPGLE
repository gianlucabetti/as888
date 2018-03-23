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
010800111206     d costc           s            120
010900020411     d cost1           s            120
011000020411     d cost2           s            120
011100020306     d cost3           s            120
011200020306     d cost4           s            120
011300020306     d cost5           s            120
011400020411     d cost5b          s            120
011500060526     d cost5c          s            120
011600060526     d cost5d          s            120
011700020306     d cost6           s            120
011800020411     d cost6a          s            120
011900090417     d cost6b          s            120
012000020411     d costi           s            120
012100020411     d costia          s            120
012200020411     d costib          s            120
012300020411     d costic          s            120
012400020411     d costid          s            120
012500020411     d costie          s            120
012600020423     d costif          s            120
012700080916     d costiH          s            120
012800080916     d costiI          s            120
012900020306     d cost7           s            120
013000070302     d cost7a          s            120
013100020411     d cost7b          s            120
013200020412     d cost7c          s            120
013300020412     d cost7d          s            120
013400020306     d cost8           s            120
013500020402     d cost8b          s            120
013600020402     d cost8c          s            120
013700020402     d cost8f          s            120
013800090417     d cost8g          s            120
013900090417     d cost8h          s            120
014000060510     d cost9           s             35
014100060322     d cost10          s             60
014200060322     d cost11          s             60
014300020313     d cost13          s             70
014400020305     d dataiso         s               d   datfmt(*iso)
014500020305     d dataeur         s               d   datfmt(*eur)
014600090511     d copiaVettore...
014700090511     d                 s               n
014800090512     D certaData       s              8s 0
014900111223     D condpag         s                     like(frncondpag)
015000020117     d prtds           ds
015100020117     d   spab                         3  0
015200020117     d   spaa                         3  0
015300020117     d   skab                         3  0
015400020117     d   skaa                         3  0
015500020117     d   line                         3  0
015600020308     D CMD             S             80    DIM(2) CTDATA PERRCD(1)              OVRPRTF
015700080916     d locemi          s             40
015800090930     d saltato         s              1    INZ(' ')
015900080916     D DVCOBADS      E DS
016000080916     D                                     INZ
016100080916     D CBAIBANDS     E DS
016200090114     d fnlvt1ds      e ds
016300090114     d ficn05ds      e ds
016400080916     ddgpi           e ds
016500110504     ***************************************************************************
016600110504     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
016700110504     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
016800110504     ***************************************************************************
016900110504     **
017000110504     ** Definizione strutture dati.
017100110504     **
017200110504      **************************************************************************
017300110504     D tibsSocI0...
017400110504     D               E DS                  QUALIFIED
017500110504     D                                     INZ
017600110504     D tibsSocO0...
017700110504     D               E DS                  QUALIFIED
017800110504     D                                     INZ
017900110504     ***************************************************************************
018000110504     **
018100110504     ** Definizione variabili modulo/programma.
018200110504     **
018300110504     ***************************************************************************
018400110504     D prmRqsOpCode...
018500110504     D                 S             10A
018600110504     D prmRpyOpCode...
018700110504     D                 S             10A
018800110504     D prmRpyIdMsg...
018900110504     D                 S             10I 0
019000110504     D prmRqsFormato...
019100110504     D                 S             10A
019200110504     D prmRqsDataSize...
019300110504     D                 S             10I 0
019400110504     D prmRpyFormato...
019500110504     D                 S             10A
019600110504     D prmRpyDataSize...
019700110504     D                 S             10I 0
019800110504
019900090511
020000090511     ***************************************************************************
020100090511     **
020200090511     ** Main.
020300090511     **
020400090511     ***************************************************************************
020500090511
020600090511     ** Stampo la copia per il vettore.
020700090511     c                   eval      copiaVettore = *on
020800090511     c                   exsr      esecuzione
020900090511
021000090511     ** Stampo la copia per il mittente.
021100090512     C*                  MOVEL(p)  CMD(1)        COMMAN           80
021200090512     C*                  Z-ADD     80            LUNG             15 5
021300090512     C*                  CALL      'QCMDEXC'
021400090512     C*                  PARM                    COMMAN
021500090512     C*                  PARM                    LUNG
021600090512     C*
021700090512     c*                  eval      copiaVettore = *off
021800090512     c*                  exsr      esecuzione
021900090512     C*
022000090512     C*                  MOVEL(p)  CMD(2)        COMMAN           80
022100090512     C*                  Z-ADD     80            LUNG             15 5
022200090512     C*                  CALL      'QCMDEXC'
022300090512     C*                  PARM                    COMMAN
022400090512     C*                  PARM                    LUNG
022500090511
022600090511     c                   seton                                        lr
022700090511
022800090511     ***************************************************************************
022900090511     **
023000090511     **
023100090511     **
023200090511     ***************************************************************************
023300090511     c     esecuzione    BEGSR
023400090511     c
023500090511     c                   reset                   prima
023600060321     c                   move      traz1         kpdr              7 0
023700020308     c
023800060321     c     kpdr          setll     titll02l
023900020308     c                   do        *hival
024000060321     c                   read      titll02l
024100060320      *fine file
024200060321     c                   if        %eof(titll02l)
024300060320     c                   leave
024400060320     c                   endif
024500060320      *trazionista singolo
024600060320     c                   if        kpdr > 0 and kpdr <> tllpdr
024700060320     c                   leave
024800060320     c                   endif
024900060324      *trazione a costo fittizio 1 euro da non inserire in elenco
025000060324     c                   if        tllili = 1
025100060324     c                   iter
025200060324     c                   endif
025300060321      *trazionista generico 9999999 valido solo se richiesto singolarmente
025400060321     c                   if        kpdr <> 9999999 and tllpdr = 9999999
025500060321     c                   leave
025600060321     c                   endif
025700060320      *leggo traino
025800060320     c     ktlt          chain     tntlt01l
025900060320     c                   if        %found(tntlt01l)
026000060321     c                   if        datae  >= tltdde and
026100060321     c                             datae  <= tltdsc
026200060321     c                   if        primo_giro = *blank
026300060321     c                   move      tllpdr        expdr
026400060321     c                   move      'X'           primo_giro        1
026500060321     c                   endif
026600060320     c                   exsr      elabora
026700060320     c                   else
026800060320     c                   iter
026900060320     c                   endif
027000020308
027100060320     c                   else
027200060320     c                   iter
027300060320     c                   endif
027400060320      *
027500020308     c                   enddo
027600090511
027700060321     c                   if        prima <> *BLANK
027800090511     C                   except    piede2
027900090511     C                   exsr      saltoPaginaF
028000090511     C                   exsr      saltoPaginaI
028100090511     C                   except    piede4
028200090511     C                   exsr      saltoPaginaF
028300090511     C                   exsr      saltoPaginaI
028400090511     C                   except    piede6
028500090511     C                   exsr      saltoPaginaF
028600090511     C                   exsr      saltoPaginaI
028700111214     c*                  if        prezzo <> 0
028800111214     C*                  except    piede8
028900111214     C*                  exsr      saltoPaginaF
029000111214     C*                  exsr      saltoPaginaI
029100111214     C*                  except    piede10
029200111214     C*                  exsr      saltoPaginaF
029300111214     C*                  exsr      saltoPaginaI
029400111214     c*                  end
029500090511     C                   except    piede12
029600090511     C                   exsr      saltoPaginaF
029700090511     C                   exsr      saltoPaginaI
029800090511     C                   except    piede14
029900090511     C                   exsr      saltoPaginaF
030000090511     C                   exsr      saltoPaginaI
030100090511     C                   except    piede16
030200090511     C                   exsr      saltoPaginaF
030300090511     C                   exsr      saltoPaginaI
030400090511     C                   except    piede18
030500090511     C                   exsr      saltoPaginaF
030600090511     C                   exsr      saltoPaginaI
030700090511     C                   except    piede20
030800090511     C                   exsr      saltoPaginaF
030900090511     C                   exsr      saltoPaginaI
031000090511     C                   except    piede22
031100090511     C                   exsr      saltoPaginaF
031200090511     C                   exsr      saltoPaginaI
031300090511     C                   except    piede24
031400090511     C                   exsr      saltoPaginaF
031500090511     C                   exsr      saltoPaginaI
031600090511     C                   except    piede26
031700090511     C                   exsr      saltoPaginaF
031800090511     C                   exsr      saltoPaginaI
031900090511     C                   except    piede28
032000090511     C                   exsr      saltoPaginaF
032100090511     C                   exsr      saltoPaginaI
032200090511     C                   except    piede30
032300090511     C                   exsr      saltoPaginaF
032400090511     C                   exsr      saltoPaginaI
032500090511     C                   except    piede32
032600090511     C                   exsr      saltoPaginaF
032700090511     C                   exsr      saltoPaginaI
032800090511     C                   except    piede34
032900090511     C                   exsr      saltoPaginaF
033000090511     C                   exsr      saltoPaginaI
033100090511     C                   except    piede36
033200090511     C                   exsr      saltoPaginaF
033300090511     C                   exsr      saltoPaginaI
033400090511     C                   except    piede38
033500090511     C                   exsr      saltoPaginaF
033600090511     C                   exsr      saltoPaginaI
033700090511     C                   except    piede40
033800090511     C                   exsr      saltoPaginaF
033900090511     C                   exsr      saltoPaginaI
034000090511     C                   except    piede42
034100090511     C                   exsr      saltoPaginaF
034200090511     C                   exsr      saltoPaginaI
034300090511     C                   except    piede44
034400090511     C                   exsr      saltoPaginaF
034500090511     C                   exsr      saltoPaginaI
034600090511     C                   except    piede46
034700090511     C                   exsr      saltoPaginaF
034800090511     C                   exsr      saltoPaginaI
034900090511     C                   except    piede48
035000090511     C                   exsr      saltoPaginaF
035100090511     C                   exsr      saltoPaginaI
035200090511     C                   except    piede50
035300090511     C                   exsr      saltoPaginaF
035400090511     C                   exsr      saltoPaginaI
035500090511     C                   except    piede52
035600090511     C                   exsr      saltoPaginaF
035700090511     C                   exsr      saltoPaginaI
035800090511     C                   except    piede54
035900090511     C                   exsr      saltoPaginaF
036000090511     C                   exsr      saltoPaginaI
036100090511     C                   except    piede56
036200090511     C                   exsr      saltoPaginaF
036300090511     C                   exsr      saltoPaginaI
036400090511     C                   except    piede58
036500090511     C                   exsr      saltoPaginaF
036600090511     C                   exsr      saltoPaginaI
036700090511     C                   except    piede60
036800090511     C                   exsr      saltoPaginaF
036900090511     C                   exsr      saltoPaginaI
037000090511     C                   except    piede62
037100090511     C                   exsr      saltoPaginaF
037200090511     C                   exsr      saltoPaginaI
037300090511     c                   except    piede70
037400090511     C                   exsr      saltoPaginaF
037500090511     C                   exsr      saltoPaginaI
037600090511     c                   except    piede72
037700090511     C                   exsr      saltoPaginaF
037800090511     C                   exsr      saltoPaginaI
037900090511     c                   except    piede74
038000090511     C                   exsr      saltoPaginaF
038100090511     C                   exsr      saltoPaginaI
038200090511     c                   except    piede80
038300090511     C                   exsr      saltoPaginaF
038400090511     C                   exsr      saltoPaginaI
038500090511     c                   except    piede82
038600090511     C                   exsr      saltoPaginaF
038700090511     C                   exsr      saltoPaginaI
038800090511     c                   except    piede84
038900090511     C                   exsr      saltoPaginaF
039000090511     C                   exsr      saltoPaginaI
039100090511     c                   except    piede86
039200090511     C                   exsr      saltoPaginaF
039300090511     C                   exsr      saltoPaginaI
039400090511     c                   except    piede88
039500090930      *
039600090511     c                   if        copiaVettore
039700090511     c                   except    firmaMitt
039800090511     c                   ENDIF
039900090930      *
040000090930     c                   if        saltato = *blank
040100090930     c                   except    PagBIANCA
040200090930     c                   end
040300090930      *
040400090511     C                   close     tntl81p
040500090511     c                   endif
040600090511
040700090511     c                   ENDSR
040800090511
040900020308      *--------------------------------------------------
041000020308     c     elabora       begsr
041100020308      *--------------------------------------------------
041200020411     c
041300060320     c                   if        tllpdr <> expdr and prima <> *blank
041400090511     C                   except    piede2
041500090511     C                   exsr      saltoPaginaF
041600090511     C                   exsr      saltoPaginaI
041700090511     C                   except    piede4
041800090511     C                   exsr      saltoPaginaF
041900090511     C                   exsr      saltoPaginaI
042000090511     C                   except    piede6
042100090511     C                   exsr      saltoPaginaF
042200090511     C                   exsr      saltoPaginaI
042300111214     c*                  if        prezzo <> 0
042400111214     C*                  except    piede8
042500111214     C*                  exsr      saltoPaginaF
042600111214     C*                  exsr      saltoPaginaI
042700111214     C*                  except    piede10
042800111214     C*                  exsr      saltoPaginaF
042900111214     C*                  exsr      saltoPaginaI
043000111214     c*                  end
043100090511     C                   except    piede12
043200090511     C                   exsr      saltoPaginaF
043300090511     C                   exsr      saltoPaginaI
043400090511     C                   except    piede14
043500090511     C                   exsr      saltoPaginaF
043600090511     C                   exsr      saltoPaginaI
043700090511     C                   except    piede16
043800090511     C                   exsr      saltoPaginaF
043900090511     C                   exsr      saltoPaginaI
044000090511     C                   except    piede18
044100090511     C                   exsr      saltoPaginaF
044200090511     C                   exsr      saltoPaginaI
044300090511     C                   except    piede20
044400090511     C                   exsr      saltoPaginaF
044500090511     C                   exsr      saltoPaginaI
044600090511     C                   except    piede22
044700090511     C                   exsr      saltoPaginaF
044800090511     C                   exsr      saltoPaginaI
044900090511     C                   except    piede24
045000090511     C                   exsr      saltoPaginaF
045100090511     C                   exsr      saltoPaginaI
045200090511     C                   except    piede26
045300090511     C                   exsr      saltoPaginaF
045400090511     C                   exsr      saltoPaginaI
045500090511     C                   except    piede28
045600090511     C                   exsr      saltoPaginaF
045700090511     C                   exsr      saltoPaginaI
045800090511     C                   except    piede30
045900090511     C                   exsr      saltoPaginaF
046000090511     C                   exsr      saltoPaginaI
046100090511     C                   except    piede32
046200090511     C                   exsr      saltoPaginaF
046300090511     C                   exsr      saltoPaginaI
046400090511     C                   except    piede34
046500090511     C                   exsr      saltoPaginaF
046600090511     C                   exsr      saltoPaginaI
046700090511     C                   except    piede36
046800090511     C                   exsr      saltoPaginaF
046900090511     C                   exsr      saltoPaginaI
047000090511     C                   except    piede38
047100090511     C                   exsr      saltoPaginaF
047200090511     C                   exsr      saltoPaginaI
047300090511     C                   except    piede40
047400090511     C                   exsr      saltoPaginaF
047500090511     C                   exsr      saltoPaginaI
047600090511     C                   except    piede42
047700090511     C                   exsr      saltoPaginaF
047800090511     C                   exsr      saltoPaginaI
047900090511     C                   except    piede44
048000090511     C                   exsr      saltoPaginaF
048100090511     C                   exsr      saltoPaginaI
048200090511     C                   except    piede46
048300090511     C                   exsr      saltoPaginaF
048400090511     C                   exsr      saltoPaginaI
048500090511     C                   except    piede48
048600090511     C                   exsr      saltoPaginaF
048700090511     C                   exsr      saltoPaginaI
048800090511     C                   except    piede50
048900090511     C                   exsr      saltoPaginaF
049000090511     C                   exsr      saltoPaginaI
049100090511     C                   except    piede52
049200090511     C                   exsr      saltoPaginaF
049300090511     C                   exsr      saltoPaginaI
049400090511     C                   except    piede54
049500090511     C                   exsr      saltoPaginaF
049600090511     C                   exsr      saltoPaginaI
049700090511     C                   except    piede56
049800090511     C                   exsr      saltoPaginaF
049900090511     C                   exsr      saltoPaginaI
050000090511     C                   except    piede58
050100090511     C                   exsr      saltoPaginaF
050200090511     C                   exsr      saltoPaginaI
050300090511     C                   except    piede60
050400090511     C                   exsr      saltoPaginaF
050500090511     C                   exsr      saltoPaginaI
050600090511     C                   except    piede62
050700090511     C                   exsr      saltoPaginaF
050800090511     C                   exsr      saltoPaginaI
050900090511     c                   except    piede70
051000090511     C                   exsr      saltoPaginaF
051100090511     C                   exsr      saltoPaginaI
051200090511     c                   except    piede72
051300090511     C                   exsr      saltoPaginaF
051400090511     C                   exsr      saltoPaginaI
051500090511     c                   except    piede74
051600090511     C                   exsr      saltoPaginaF
051700090511     C                   exsr      saltoPaginaI
051800090511     c                   except    piede80
051900090511     C                   exsr      saltoPaginaF
052000090511     C                   exsr      saltoPaginaI
052100090511     c                   except    piede82
052200090511     C                   exsr      saltoPaginaF
052300090511     C                   exsr      saltoPaginaI
052400090511     c                   except    piede84
052500090511     C                   exsr      saltoPaginaF
052600090511     C                   exsr      saltoPaginaI
052700090511     c                   except    piede86
052800090511     C                   exsr      saltoPaginaF
052900090511     C                   exsr      saltoPaginaI
053000090511     c                   except    piede88
053100090930      *
053200090511     c                   if        copiaVettore
053300090511     c                   except    firmaMitt
053400090511     c                   ENDIF
053500060321     c                   move      tllpdr        expdr             7 0
053600060321     c                   clear                   prima             1
053700090511     C                   close     tntl81p
053800060320     c                   endif
053900060320
054000060320     c                   if        prima = *blank
054100090511     C                   OPEN      tntl81p
054200060321     c                   z-add     0             page
054300060321     c                   move      expdr         fornit
054400080916     c* imposto costanti nel caso non reperisco anagrafico
054500080916     c                   eval      costia= 'Ragione sociale ___________________+
054600080916     c                             ____________________________________________'
054700080916
054800080916     c                   eval      costib= 'Indirizzo__________________________+
054900080916     c                             ____________________________________________'
055000080916
055100080916     c                   eval      costic= 'CAP______________ Città____________+
055200080916     c                             ____________________________________________'
055300080916
055400080916     c                   eval      costif= 'Recapito __________________________+
055500080916     c                             ____________________________________________'
055600080916     c                   eval      costid= 'Partita IVA _______________________+
055700080916     c                             ____________________________________________'
055800080916     c                   eval      cost8c= '|__|__|__|__|  -
055900080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
056000080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
056100060321      * decodifico fornitore
056200060321     C                   clear                   ragsvet
056300080918     c                   movel     *all'_'       cdpdescr
056400060321     C     kfrn          chain     anfrn01l
056500060321     c                   if        %found(anfrn01l)
056600111223     c                   if        datae = 20120111 and frncondpag = 'B70'
056700111223     c                   eval      condpag = 'B55'
056800111223     c                   else
056900111223     c                   eval      condpag = frncondpag
057000111223     c                   end
057100080918     C     kcdp          chain     ancdp01l
057200060321     C     frnsogg       chain     ansog01l
057300060321     c                   if        %found(ansog01l)
057400080919     C                   MOVEL     sogdes        ragsvet          44
057500060321     c                   endif
057600060321     c                   endif
057700080918     c                   eval      cost8b= %trimr(cdpdescr) +' '+
057800080918     c                             'disposto su codice IBAN:'
057900080916     c* indirizzo
058000080916     c                   clear                   tipo
058100080916     c                   clear                   codice
058200080916     C     Kind          CHAIN     anind01l
058300080916     c                   if        %found(anind01l)
058400080916     C                   eval      costia = 'Ragione sociale ' + sogdes
058500080916     C                   eval      costib = 'Indirizzo ' + indindriz
058600080916     c                   if        indprov <> ' '
058700080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
058800080916     c                   else
058900080916     C                   eval      locemi = indlocalit
059000080916     c                   end
059100080916     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
059200080916     c                             locemi
059300080916     c                   end
059400080916     C                   eval      costid = 'Partita IVA ' + sogpartiva
059500091218      **
059600091218      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
059700091218      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
059800091218      **
059900091218      **  Aggiunge il Codice Fiscale
060000091218     c                   if        sogcdfisc <> ' ' and indprov <> ' '
060100091218     c                   eval      costiD = %Trim(costiD) +
060200091218     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
060300091218     C                             ': ' + %trimr(sogcdfisc)
060400091218     c                   end
060500091218      **
060600091218     c                   if        indtelex <> ' ' and
060700091218     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
060800091218     c                   eval      costiD = %Trim(costiD) + ' ' + indtelex
060900091218     c
061000091218     c                   end
061100091218      **
061200080916     c* indirizzo recapito
061300080916     C                   SETOFF                                       89
061400080916     c                   eval      tipo ='IF'
061500080916     c                   eval      codice ='000'
061600080916     C     Kind          CHAIN     anind01l
061700080916     c                   if        %found(anind01l)
061800080916     C                   SETON                                        89
061900080916     c                   eval      costif= 'Recapito ' + sogdes
062000080916     C                   eval      costiH = indindriz
062100080916     c                   if        indprov <> ' '
062200080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
062300080916     c                   else
062400080916     C                   eval      locemi = indlocalit
062500080916     c                   end
062600080916     C                   eval      costiI = indcap + ' ' +
062700080916     c                             locemi
062800080916     c                   end
062900080916     c* reperisco IBAN
063000080916     C                   RESET                   DVCOBADS
063100080916     C                   RESET                   CBAIBANDS
063200080916
063300080916     C                   EVAL      CoBSoc = socie
063400080916     C                   EVAL      CoBCOF = 'F'
063500080916     C                   EVAL      CoBKsc = fornit
063600080916     C                   CALL      'DVCOBA'
063700080916     C                   PARM                    DVCOBADS
063800080916A0846C                   PARM                    CBAIBANDS
063900080916     C                   IF        CoBErr = *OFF
064000080916     C                   EVAL      cost8c = CBaStato + CbaCheck
064100080916     C                             + %TRIMR(CBaCoorI)
064200080916     c                   end
064300110504      *
064400110504     C*** SPOSTATA QUI LA DECODIFICA DELLA RAGIONE SOCIALE MITTENTE
064500110504     c                   exsr      decod_societa
064600110504      *
064700110504      *
064800060321      *stampa testata allegato
064900020306     c                   except    testata
065000090512     c                   except    tesDtCerta
065100060320     c                   except    testo
065200060320     c                   move      'X'           prima
065300020306     c                   endif
065400090511     c*                  if        line > 60
065500090511     c*                  except    testata
065600090511     c*                  endif
065700090511     C                   exsr      saltoPaginaF
065800090511     C                   exsr      saltoPaginaI
065900060320      *decodifica partenza
066000060320     c     tlttfp        chain     azorg01l
066100060320     c                   if        %found(azorg01l)
066200060320     c                   movel     orgdes        partenza         20
066300060320     c                   else
066400060320     c                   clear                   partenza
066500060320     c                   endif
066600060320      *decodifica arrivo
066700060320     c     tlttfa        chain     azorg01l
066800060320     c                   if        %found(azorg01l)
066900060321     c                   movel     orgdes        arrivo           20
067000060320     c                   else
067100060321     c                   clear                   arrivo
067200060320     c                   endif
067300090114      * reperimento kilometri tappe traino
067400090402     c*mm                clear                   fnlvt1ds
067500090402     c*mm                eval      it1trn = tlltrn
067600090402     c*mm                eval      it1dde = datae
067700090402     c*mm                call      'FNLVT1R'
067800090402     C*mm                parm                    fnlvt1ds
067900090402     c*mm                if        ot1err = *blank
068000090402     c*mm                eval      totkm  = ot1km
068100090402     c*mm                else
068200090402     c*mm                clear                   totkm             6 0
068300090402     c*mm                endif
068400060320      * stampa riga listino attiva
068500060320     c                   except    dettaglio
068600060320      *reperimento nota
068700060320     c     ktln          chain     tntln01l
068800060320     c                   if        %found(tntln01l)
068900060320     c                   except    dettanote
069000060320     c                   endif
069100060320
069200020308     c                   endsr
069300090511
069400090511     ***************************************************************************
069500090511     **
069600090511     ** Salto pagina: fine pagina.
069700090511     **
069800090511     ***************************************************************************
069900090511     C     saltoPaginaF  BEGSR
070000090511     c                   if        line > 60 AND copiaVettore
070100090511     c                   except    firmaMitt
070200090930     c                   eval      saltato = 'S'
070300090511     c                   ENDIF
070400090511     C                   ENDSR
070500090511
070600090511     ***************************************************************************
070700090511     **
070800090511     ** Salto pagina: inizio nuova pagina.
070900090511     **
071000090511     ***************************************************************************
071100090511     C     saltoPaginaI  BEGSR
071200090511     c                   if        line > 60
071300090511     c                   except    testata
071400090512     c                   if        %rem(page : 2) = 1
071500090930     c                   clear                   saltato
071600090512     c                   except    tesDtCerta
071700090512     c                   except    saltino
071800090511     c                   ENDIF
071900090511     c                   endif
072000090511     C                   ENDSR
072100020304
072200011026     c*-------------------------------------------------
072300011026     c     *inzsr        begsr
072400011026     c*-------------------------------------------------
072500020304     c     *entry        plist
072600020304     c                   parm                    kpjba
072700020304     c                   movel     kpjbu         param
072800020304
072900060320     c     kfrn          klist
073000060320     c                   kfld                    socie
073100060320     c                   kfld                    fornit            8
073200060320     c                   move      *all'0'       fornit
073300080918     c     kcdp          klist
073400080918     c                   kfld                    socie
073500111223     c                   kfld                    condpag
073600080916     c     kind          klist
073700080916     c                   kfld                    frnsogg
073800080916     c                   kfld                    tipo              2
073900080916     c                   kfld                    codice            3
074000021203
074100060320     c     ktlt          klist
074200060320     c                   kfld                    tlltrn
074300060320     c                   kfld                    tlldde
074400020402
074500060320     c     ktln          klist
074600060320     c                   kfld                    tlltrn
074700060320     c                   kfld                    tlldde
074800060320     c                   kfld                    numfe             3 0
074900060320     c                   kfld                    numpro            3 0
075000060322     c                   z-add     1             numpro
075100060320
075200020218     C                   time                    w0120            14 0
075300020218     C                   move      w0120         wdat              8 0
075400020308      * data da impostare su data stampa e convalida se lancio = 'C'
075500020308     c                   move      wdat          dataeur
075600020308     c                   move      dataeur       dataiso
075700020308     c                   move      dataiso       dataoggi          8 0
075800020308      * data da elaborare
075900111223     c                   if        datae = 20120111
076000111223     c                   eval      alladata = 01012012
076100111223     c                   else
076200060321     c                   move      datae         dataiso
076300020304     c                   move      dataiso       dataeur
076400020304     c                   move      dataeur       alladata          8 0
076500111223     c                   end
076600090512      * data certa
076700090512     c                   move      dataCerta     dataiso
076800090512     c                   move      dataiso       dataeur
076900090512     c                   move      dataeur       certaData
077000011026
077100020117     c                   z-add     99            line
077200020305     c                   move      *all'_'       separa          100
077300060321      * ragione sociale società mittente
077400110504      *****
077500110504     c**** socie         chain     ansif01l
077600110504     c****               if        %found(ansif01l)
077700110504     c****               movel     sifrgs        ragsmit          30
077800110504     c****               endif
077900110504      *****
078000110504      *****  Decodifica ragione sociale mittente SPOSTATA PRIMA DELLA EXCPT
078100110504      *****
078200080916     c* prezzo iniziale carburante legge 133
078300090114     c                   clear                   ficn05ds
078400090114     c                   call      'FICN05R'
078500090114     c                   parm                    ficn05ds
078600090114     c                   eval      dgpi = o05gpi
078700090114     c                   z-add     D§GPIGPI      prezzo            5 3
078800090114
078900090417     c                   eval      costt = 'ALLEGATO  B   IN VIGORE DAL'
079000020412     c
079100080916     c* imposto costanti nel caso non reperisco anagrafico
079200080916     c                   eval      costia= 'Ragione sociale ___________________+
079300080916     c                             ____________________________________________'
079400080916
079500080916     c                   eval      costib= 'Indirizzo__________________________+
079600080916     c                             ____________________________________________'
079700080916
079800080916     c                   eval      costic= 'CAP______________ Città____________+
079900080916     c                             ____________________________________________'
080000080916
080100080916     c                   eval      costif= 'Recapito __________________________+
080200080916     c                             ____________________________________________'
080300080916     c                   eval      costid= 'Partita IVA _______________________+
080400080916     c                             ____________________________________________'
080500080916     c                   eval      cost8c= '|__|__|__|__|  -
080600080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
080700080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
080800020411     c*costanti pretariffa
080900020411     c                   eval      costa = 'Allegato tariffario al contratto +
081000060321     c                             di trasporto sottoscritto tra la società '
081100060321     c                   eval      costa2= '(di seguito denominata MITTENTE) e '
081200060321     c                   eval      costa2b= 'codice fornitore'
081300060321     c                   eval      costa3= '(di seguito denominata VETTORE)'
081400060321     c
081500060321     c                   eval      cost1 = 'Le parti come convenuto all''art.+
081600060321     c                             10 del suddetto contratto di trasporto, con+
081700111212     c                             cordano i corrispettivi per i trasporti,'
081800111206     c                   eval      cost2 = 'comprensivi delle eventu-
081900111206     c                             ali operazioni connesse al trasporto stesso,-
082000111206     c                              nonchè di quals+
082100111206     c                             iasi onere necessario per'
082200111206     c                   eval      costb = 'l''esecuzione -
082300111206     c                             del trasporto (pedaggi, diritti di +
082400111206     c                             transito, equipaggiamento del veicolo, -
082500111206     c                             tempi di carico e scarico,'
082600111206     c                   eval      costc ='ecc.) secondo la seguente tariffa :'
082700020306
082800020411     c*costanti piede
082900020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
083000020411     c                             he i corrispettivi indicati nelle sopraccita+
083100020411     c                             te tariffe possano essere oggetto di'
083200020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
083300020411     c                             one, a seguito di modifiche pattuite tra le +
083400020412     c                             parti del progetto operativo'
083500020412     c                   eval      cost3c= 'originariamente concordato.'
083600080916     c                   eval      cost3d= 'Il vettore dichiara espressamente -
083700080916     c                             che i corrispettivi di cui al presente alleg-
083800080916     c                             ato sono stati concordati'
083900080916     c                   eval      cost3e= 'con il mittente sulla base del -
084000080916     c                             costo del carburante pari a euro xx,xxx/lt.'
084100090417     c                   eval      cost3f= 'Ai sensi e per gli effetti dell''ar+
084200090417     c                             t. 6, comma 3 lett. e-bis D. Lgs. 286/05, +
084300090417     c                             il contratto di trasporto '
084400090417     c                   eval      cost3g= 'di cui il presente costituisce alle+
084500090417     c                             gato, deve intendersi integrato con l''indi+
084600090417     c                             cazione dei tempi massimi '
084700090417     c                   eval      cost3h= 'di carico e scarico che le stesse +
084800090417     c                             parti indicano in complessive ore 2 (due) +
084900090417     c                             in relazione a ciascun'
085000111206     c                   eval      cost3i= 'trasporto da effettuars+
085100090417     c                             i nell''arco della giornata.'
085200080916
085300020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
085400020402     c                              società da questi delegata ad emetter+
085500020411     c                              e alla fine di ogni mese in nome e per '
085600020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
085700111206     c                             a relativa ai trasporti ed alle o+
085800111212     c                             perazioni connesse effettuati in tale perio-
085900111206     c                             do.'
086000111206     c                   eval      cost5b= 'Tale fattura com-
086100060526     c                             prenderà anche i corrispettivi per le presta+
086200060526     c                             zioni di trasporto non previste'
086300020306
086400090417     c                   eval      cost5c= 'dal presente allegato B, alle quali-
086500060526     c                              le parti in ogni caso riconoscono la piena +
086600060526     c                             validità ed efficacia del '
086700060526
086800060526     c                   eval      cost5d= 'contenuto degli articoli 10 e 14 -
086900060526     c                             del contratto di trasporto.'
087000060526
087100020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
087200060321     c                             dei fogli viaggio  che contengono in dettagl+
087300111206     c                             io i trasporti e le '
087400020411
087500111212     c                   eval      cost6a= 'operazioni connesse effettuati.'
087600090417     c                   eval      cost6b= 'La fattura sarà emessa con IVA +
087700090417     c                             ad esigibilità immediata.'
087800020411
087900020411     c                   eval      costi = 'L''intestazione del vettore da rip+
088000020411     c                             ortare sulla fattura emessa è:'
088100020411
088200020411     c                   eval      costie= 'Il vettore in caso di variazione de+
088300020412     c                             ll''intestazione ne darà immediata comunic+
088400020412     c                             azione al mittente.'
088500020411
088600020423     c                   eval      cost7 = 'Il presente allegato decorre d+
088700070302     c                             alla data del __________,annulla e sostituis+
088800070302     c                             ce qualsiasi precedente patto verbale o'
088900070302     c                   eval      cost7a= 'scritto relativo al corrispettivo +
089000111206     c                             per l''esecuzione di trasporti +
089100070302     c                             e costituisce parte integrante del '
089200070302     c                   eval      cost7b= 'contratto di trasporto suddetto.'
089300020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
089400020412     c                             la piena validità ed efficacia del contenut+
089500060321     c                             o degli articoli 10 e 14 del'
089600020412     c                   eval      cost7d='contratto di trasporto.'
089700020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
089800020411     c                             pagamento dei corrispettivi fatturati verrà +
089900080918     c                             effettuato dalla mittente mediante'
090000020402
090100020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
090200020411     c                             lle coordinate bancarie ne darà +
090300020412     c                             immediata comunicazione al mittente.'
090400090417     c                   eval      cost8g='Il vettore approva espressamente il +
090500090417     c                             contenuto del presente allegato ed in partic+
090600090417     c                             olare le pattuizioni'
090700090417     c                   eval      cost8h= 'relative alla quantificazione, +
090800090417     c                             modalità e termini di pagamento dei +
090900090417     c                             corrispettivi.'
091000090417
091100060510     c                   eval      cost9 = 'Bologna li, _______________________'
091200020411     c                   eval      cost10= 'Firma del Vettore _________________+
091300060322     c                             _________________________'
091400090511     c*                  eval      cost11= 'Firma del Mittente_________________+
091500090511     c*                            _________________________'
091600020313     c                   eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
091700020313     c                              VALIDA CON I REFERIMENTI RICHIESTI'
091800020313
091900011026     c                   endsr
092000011129     C*--------------------------------------------------------------------
092100110504     C* decodifica la società
092200110504     C*--------------------------------------------------------------------
092300110504     C     Decod_societa begSR
092400110504      *
092500110504     c                   move      DataE         dataiso
092600110504     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
092700110504     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
092800110504     C                   RESET                   tibsSocI0
092900110504     C                   EVAL      tibsSocI0.idSocieta = socie
093000110504     C                   EVAL      tibsSocI0.DTVALIDITA = dataiso
093100110504     C
093200110504     C                   CALL      'TIBSSOCR'
093300110504     C                   PARM      'GETANAGRAF'  prmRqsOpCode
093400110504     C                   PARM      *BLANK        prmRpyOpCode
093500110504     C                   PARM      *ZERO         prmRpyIdMsg
093600110504     C                   PARM      'TIBSSOCI0'   prmRqsFormato
093700110504     C                   PARM                    tibsSocI0
093800110504     C                   PARM                    prmRqsDataSize
093900110504     C                   PARM      'TIBSSOCO0'   prmRpyFormato
094000110504     C                   PARM                    tibsSocO0
094100110504     C                   PARM                    prmRpyDataSize
094200110504      *
094300110504     c                   clear                   ragsmit          30
094400110504     c                   if         PRMRPYIDMSG >= 0
094500110504     c                              and TIBSSOCO0.IDSOCIETA <> *blank
094600110504     c                   eval       ragsmit = tibsSocO0.RAGSOCIALE
094700110504     c                   end
094800110504      *
094900110504     C                   ENDSR
095000110504     C************************************************************
095100090511
095200090511     Otntl81p   E            testata          02
095300060421     o                       costt               27
095400090512     o                       alladata          +  2 '0  .  .    '
095500090930     o*******                                   108 'Pag.'
095600090930     O                       page          z    115
095700090511     O*         E            testata     0
095800090511     o*                      costt               27
095900090511     o*                      alladata          +  2 '  /  /    '
096000090511     O*         E            testata     0
096100090511     o*                      costt               27
096200090511     o*                      alladata          +  2 '  /  /    '
096300090512     O          E            tesDtCerta       07
096400090512     O                       certaData           47 '0  .  .    '
096500020411      *testo previsto per collegamento al contratto
096600090511     O          E            testo            11
096700060321     o                       costa               74
096800060321     o                       ragsmit            110
096900020411     O          E            testo       1
097000060321     o                       costa2              35
097100060321     o                       ragsvet             80
097200060321     o                       costa2b            100
097300060321     o                       frnksc             110
097400020411     O          E            testo       1
097500020411     o                       costa3             120
097600020306     O          E            testo       1
097700020411     o                       cost1              120
097800020306     O          E            testo       1
097900020411     o                       cost2              120
098000020306      *
098100020402     O          E            testo       1
098200020411     o                       costb              120
098300111206     O          E            testo       1
098400111206     o                       costc              120
098500020402      *
098600060320     O          E            testo       1
098700020305     o                       separa             100
098800060320     O          E            testo       2
098900060320     o                                           10 'Nr.Traino'
099000060321     o                                           21 'Partenza'
099100060321     o                                           45 'Arrivo'
099200060321     o                                           77 'Importo'
099300090402     o*mm                                        87 'Km.'
099400090511     O*         E            testo       0
099500090511     o*                                          10 'Nr.Traino'
099600090511     o*                                          21 'Partenza'
099700090511     o*                                          45 'Arrivo'
099800090511     o*                                          77 'Importo'
099900090402     o*mm                                        87 'Km.'
100000090511     O*         E            testo       0
100100090511     o*                                          10 'Nr.Traino'
100200090511     o*                                          21 'Partenza'
100300090511     o*                                          45 'Arrivo'
100400090511     o*                                          77 'Importo'
100500090402     o*mm                                        87 'Km.'
100600060320      *dettaglio
100700060320     O          E            dettaglio   1
100800060320     o                       tlltrn        z      8
100900060322     o                       partenza          +  5
101000060322     o                       arrivo            +  6
101100060320     o                       tllili        2   +  2
101200090402     o*mm                    totkm         2     87
101300090511     O*         E            dettaglio   0
101400090511     o*                      tlltrn        z      8
101500090511     o*                      partenza          +  5
101600090511     o*                      arrivo            +  6
101700090511     o*                      tllili        2   +  2
101800090402     o*mm                    totkm         2     87
101900090511     O*         E            dettaglio   0
102000090511     o*                      tlltrn        z      8
102100090511     o*                      partenza          +  5
102200090511     o*                      arrivo            +  6
102300090511     o*                      tllili        2   +  2
102400090402     o*mm                    totkm         2     87
102500060320     O          E            dettanote   1
102600060324     o                                           10 'Percorso'
102700060322     o                       tlntno              73
102800020411      *testo previsto per collegamento al contratto piede
102900020411      * parte prima
103000090511     O          E            piede2      2
103100020306     o                       cost3              120
103200090511     O          E            piede4      1
103300020411     o                       cost3b             120
103400090511     O          E            piede6      1
103500020412     o                       cost3c             120
103600090511     O          E            piede8      1
103700080916     o                       cost3D             120
103800090511     O          E            piede10     1
103900080916     o                       cost3E             120
104000080916     o                       prezzo        2     70
104100090511     O          E            piede12     1
104200090417     o                       cost3f             120
104300090511     O          E            piede14     1
104400090417     o                       cost3g             120
104500090511     O          E            piede16     1
104600090417     o                       cost3h             120
104700090511     O          E            piede18     1
104800090417     o                       cost3i             120
104900090511     O          E            piede20     1
105000020306     o                       cost4              120
105100090511     O          E            piede22     1
105200020306     o                       cost5              120
105300090511     O          E            piede24     1
105400020411     o                       cost5b             120
105500090511     O          E            piede26     1
105600060526     o                       cost5c             120
105700090511     O          E            piede28     1
105800060526     o                       cost5d             120
105900020411      * parte seconda
106000090511     O          E            piede30     1
106100020306     o                       cost6              120
106200090511     O          E            piede32     1
106300020411     o                       cost6a             120
106400090511     O          E            piede34     1
106500090417     o                       cost6b             120
106600020411      * parte terza
106700090511     O          E            piede36     1
106800020411     o                       costi              120
106900090511     O          E            piede38     1
107000020411     o                       costia             120
107100090511     O          E            piede40     1
107200020411     o                       costib             120
107300090511     O          E            piede42     1
107400020411     o                       costic             120
107500090511     O          E            piede44     1
107600080916     o                       costid             120
107700090511     O          E            piede46     1
107800020423     o                       costif             120
107900090511     O          E    89      piede48     1
108000080916     o                       costiH             120
108100090511     O          E    89      piede50     1
108200080916     o                       costiI             120
108300090511     O          E            piede52     1
108400020411     o                       costie             120
108500020411      * parte quarta
108600090511     O          E            piede54     1
108700020306     o                       cost7              120
108800090512     o                       alladata            54 '0  .  .    '
108900090511     O          E            piede56     1
109000070302     o                       cost7a             120
109100090511     O          E            piede58     1
109200020411     o                       cost7b             120
109300090511     O          E            piede60     1
109400020412     o                       cost7c             120
109500090511     O          E            piede62     1
109600020412     o                       cost7d             120
109700020411      * parte quinta
109800090511     O          E            piede70     1
109900020306     o                       cost8              120
110000090511     O          E            piede72     1
110100020402     o                       cost8b             120
110200090511     O          E            piede74     1
110300020402     o                       cost8c             120
110400090511     O          E            piede80     1
110500020402     o                       cost8f             120
110600090511     O          E            piede82     1
110700090417     o                       cost8g             120
110800090511     O          E            piede84     1
110900090417     o                       cost8h             120
111000020411      * parte sesta
111100090511     O          E            piede86     2
111200060510     o                       cost9               35
111300090511     O          E            piede88     2
111400090511     o*                      cost11              60
111500060321     o                       cost10             120
111600090511     o*
111700090511     O          E            firmaMitt        66
111800090511     O                       ragsmit             85
111900090511     o*
112000090511     O          E            saltino          10
112100090930     o*
112200090930     O          E            PagBIANCA  05
112300090511     o                                            1 ' '
112400090930     o                                           +1 'PAGINA VOLUTAMENTE LASCIAT'
112500090930     o                                           +0 'A IN BIANCO'
112600090930     o
112700020313**
112800090511OVRPRTF FILE(TNTL81P) FRONTOVL(TNTL81FX) BACKOVL(*NONE)
112900090511DLTOVR FILE(TNTL81P)
