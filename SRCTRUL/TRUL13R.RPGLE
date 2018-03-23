000100030221     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200030221
000300030326      * TRUL13R  *---------------------------------------------------*
000400030326      * - CALCOLA PRIMO GIORNO LAVORATIVO DISPONIBILE
000500020412      *--------------------------------------------------------------*
000600030221
000700030326     fAzcln01l  if   e           k disk
000800030224
000900030224     d msg             s             78    dim(10) ctdata perrcd(1)
001000020412
001100030326     d kClnTfp         s                   Like(ClnTfp)
001200030326     d kClnTfa         s                   Like(ClnTfa)
001300030326     d kClnAnn         s                   Like(ClnAnn)
001400030326     d kClnMes         s                   Like(ClnMes)
001500030326
001600030326     d dataiso         s               d   datfmt(*iso)
001700030326     d giorni          s              2  0
001800030326     d resto           s              2  0
001900030326     d wn60            s              6  0
002000030326     d wn80            s              8  0
002100030326
002200030326     d                 ds
002300030326     d  ds_anno                1      4  0
002400030326     d  ds_mese                5      6  0
002500030326     d  ds_giorno              7      8  0
002600030326     d  ds_data                1      8  0
002700030326
002800030326     d                 DS
002900030326     d tempo                           Z   INZ(Z'1901-01-01-01.00.00.000000')
003000030326     d  data                           D   DATFMT(*ISO) OVERLAY(tempo)
003100030326     d  ora                            T   TIMFMT(*ISO) OVERLAY(tempo:12)
003200030326
003300030326     d clnpom          ds
003400030326     d  pom                    1     31    dim(31)
003500020412
003600030326     d Trul13ds      e ds
003700020412
003800020412      *---------------------------------------------------------------
003900030221     c     *Entry        Plist
004000030326     c                   Parm                    Trul13ds
004100030326
004200030326     c     kAzcln        Klist
004300030326     c                   Kfld                    kClnTfp
004400030326     c                   Kfld                    kClnTfa
004500030326     c                   Kfld                    kClnAnn
004600030326     c                   Kfld                    kClnMes
004700020412
004800030326      * I13Tla = ' '  Elaboro e chiudo con RETURN
004900030326      * I13Tla = 'L'  Elaboro e chiudo con LR
005000030326      * I13Tla = 'C'            chiudo con LR  solo per chiusura file
005100030221
005200030326If  1c                   If        I13Tla = 'C'
005300030221     c                   Eval      *InLr = *On
005400020412
005500030221   x1c                   Else
005600030326
005700030326      * se non è stato passato il tipo chiamata assumo 'P' di partenza
005800030326     c                   If        I13Mod = *Blanks
005900030326     c                   Eval      I13Mod = 'P'
006000030326     c                   EndIf
006100030326      * se non è stato passato il p.o. ritorno msg di errore
006200030326     c                   If        I13Fil = *Zeros
006300030326     c                   Eval      O13Err = 'E'
006400030326     c                   Eval      O13Msg = Msg(1)
006500030326     c                   EndIf
006600030326      * se non è stata passata la data assumo data del giorno
006700030326     c                   If        I13Din = *Zeros
006800030326     c                   Eval      I13Din = *Date
006900030326     c                   EndIf
007000030326      * se non sono stati passati nè i giorni nè le ore ritorno msg di errore
007100030326     c                   If        I13Gio = *Zeros and I13Ore = *Zeros
007200030326     c                   Eval      O13Err = 'E'
007300030326     c                   Eval      O13Msg = Msg(2)
007400030326     c                   GoTo      Fine
007500030326     c                   EndIf
007600030326      * prendo le ore passate e calcolo i giorni
007700030326     c                   If        I13Ore > *Zeros
007800030326     c     I13Ore        Div       24            giorni
007900030326     c                   Mvr                     resto
008000030326     c                   EndIf
008100030326      * prendo i giorni - i giorni vincono sulle ore
008200030326     c                   If        I13Gio > *Zeros
008300030326     c                   Eval      giorni = I13Gio
008400030326     c                   Clear                   resto
008500030326     c                   EndIf
008600030326      * imposto i primi campi chiave del calendario
008700030326     c                   If        I13Mod = 'P'
008800030326     c                   Eval      kClnTfp = I13Fil
008900030326     c                   Clear                   kClnTfa
009000030326     c                   EndIf
009100030326     c                   If        I13Mod = 'A'
009200030326     c                   Eval      kClnTfa = I13Fil
009300030326     c                   Clear                   kClnTfp
009400030326     c                   EndIf
009500030326      * alla data iniziale devo aggiungere i giorni
009600030326     c                   Move      I13Din        dataiso
009700030326Do  2c                   Do        giorni
009800030326      * non devo considerare i giorni festivi
009900030326Do  3c                   Do        *Hival
010000030326     c                   Adddur    1:*d          dataiso
010100030326     c                   Move      dataiso       ds_data
010200030326     c                   Eval      kClnAnn = ds_anno
010300030326     c                   Eval      kClnMes = ds_mese
010400030326     c     kAzcln        Chain     Azcln01l
010500030326      * se non trovo il calendario ritorno msg di errore
010600030326If  4c                   If        Not %Found(Azcln01l)
010700030326     c                   Eval      O13Err = 'E'
010800030326     c                   Eval      O13Msg = Msg(3)
010900030326     c                   Leave
011000030326    4c                   EndIf
011100030327     c                   If        POM(ds_giorno) =  'F'
011200030326     c                   Iter
011300030326    4c                   EndIf
011400030326     c                   Leave
011500030326    3c                   EndDo
011600030326    2c                   EndDo
011700030326      * sono avanzate delle ore devo tenerne conto
011800030326If  2c                   If        resto > *Zeros
011900030326     c                   Clear                   wn60
012000030326     c                   Clear                   wn80
012100030326     c                   Move      ds_data       wn80
012200030326     c                   Movel     I13Hin        wn60
012300030326     c     *iso          Move      wn80          data
012400030326     c     *iso          Move      wn60          ora
012500030326     c                   Adddur    resto:*h      tempo
012600030326     c     *iso          Move      data          wn80
012700030326     c     *iso          Move      ora           wn60
012800030326     c                   Move      wn80          dataiso
012900030326      * se festivo cerco il primo giorno feriale sucessivo
013000030326Do  3c                   Do        *hival
013100030326     c                   Move      dataiso       ds_data
013200030326     c                   Eval      kClnAnn = ds_anno
013300030326     c                   Eval      kClnMes = ds_mese
013400030326     c     kAzcln        Chain     Azcln01l
013500030326      * se non trovo il calendario ritorno msg di errore
013600030326If  4c                   If        Not %Found(Azcln01l)
013700030326     c                   Eval      O13Err = 'E'
013800030326     c                   Eval      O13Msg = Msg(3)
013900030326     c                   Leave
014000030326    4c                   EndIf
014100030327     c                   If        POM(ds_giorno) =  'F'
014200030326     c                   Adddur    1:*d          dataiso
014300030326     c                   Iter
014400030326    4c                   EndIf
014500030326     c                   Leave
014600030326    3c                   EndDo
014700030326    2c                   EndIf
014800030326      * imposto la data finale
014900030326     c                   Move      ds_data       O13Dfi
015000030326      * imposto l'ora finale
015100030326     c                   If        wn60 > *Zeros
015200030326     c                   Movel     wn60          O13Hfi
015300030326     c                   Else
015400030326     c                   Move      I13Hin        O13Hfi
015500030326     c                   EndIf
015600030326
015700030326     c     Fine          Tag
015800030326
015900030326    2c                   If        I13Tla = 'L'
016000030326     c                   Eval      *InLR = *On
016100030326   X2c                   Else
016200030326     c                   Return
016300030326    2c                   EndIf
016400030326
016500030326    1c                   EndIf
016600030326
016700030224** MSG  Lungh. 78                                                            *
016800030326TRUL13R - Manca il punto operativo                                                  1
016900030326TRUL13R - Mancano i giorni o le ore da sommare alla data iniziale                   2
017000030326TRUL13R - Non trovato calendario                                                    3
