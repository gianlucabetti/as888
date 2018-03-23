000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200090202      *-------------------------------------------------------------------------------
000300090202      * SUDDIVIDE in singoli campi una stringa
000400090202      *  i cui campi sono contigui e separati da un carattere particolare.
000500090202      *    AAAAAAAAA+BBBB+CCCC,CC+DDDDDDD+EEEEEE+....
000600090202      *  diventano così:
000700090202      *    AAAAAAAAA         --> campo1
000800090202      *    BBBB              --> campo2
000900090202      *    CCCC,CC           --> campo3
001000090202      *    DDDDDDD           --> campo4
001100090202      *    EEEEEE            --> campo5
001200090202      * ------------------------------------------------------------------------
001300090202      * Riceve:
001400090202      *  la stringa da suddividere       in  DATI
001500090202      *  in separatore x split dei campi in  Separa_segmen "+" (nell'esempio)
001600090202      *  i decimali sono separati da ,/. in  Decimal_Separ "," (nell'esempio)
001700090202      *
001800090202      * Restituisce:
001900090202      *   Nr_campi           --> Nr. di quanti campi c'erano nella stringa
002000090202      *     delle schiere correlate:
002100090202      *   Campi_Record       --> Le variabili contenute nei campi
002200090202      *   Campi_Lunghi       --> Lunghezza di ogni singolo campo
002300090202      *   Campi_Numerici     --> se il campo è Numerico o Alfa
002400090202      *   Campi_Decimali     --> se numerico con quanti decimali
002500090202      *
002600090202      *-------------------------------------------------------------------------------
002700050713      * ?Definizioni x gestire lo split del record passato
002800110621     d  esito          s              1
002900110302     D  Lun_dati       s              5u 0 INZ(0)
003000050629     D in_X_prima      s              5i 0 INZ(0)
003100050629     D  in_X           s              5i 0 INZ(0)
003200050629     D  da_X           s              5i 0 INZ(0)
003300050629     D Campo_Nr        s              3u 0 INZ(0)
003400090130     D Campo           s           1000    INZ(' ')
003500110302     D Campo_Lungo     s              5i 0 INZ(0)
003600050629     D Campo_Vuoto     s              1    INZ(' ')
003700050629     D Campo_Numerico  s              1    INZ(' ')
003800050629     D Campo_Decimali  s              1    INZ(' ')
003900110302     D Num_Decimali    s              3i 0 INZ(0)
004000050629     D  X_Byte         s              5i 0 INZ(0)
004100050629     D    Byte         s              1    INZ(' ')
004200050713      *
004300050713      * ?Dati di Input  (Record ricevuto dal pgm chiamante e da splittare)
004400090130     D  Dati           s           2048
004500050713     D Decimal_Separ   s              1
004600090130     D Separa_segmen   s              1
004700090204     D Lung_dati       s              5i 0
004800050610      *
004900050713      * ? Schiere di Output / Campi di Output
005000050713      * ? I dati da ripassare al chiamante devono essere :
005100050713      * ? 1 - il nr.dei campi splittati
005200090130      * ? 2 - una schiera contenente i campi splittati (lunghi max.1000 chrs)
005300050713      * ? 3 - una schiera che definisce la lunghezza di ciascun campo
005400050713      * ? 4 - una schiera che dice se il campo è numerico o alfa
005500050713      * ? 5 - una schiera che dice se ci sono dei decimali (se numerico)
005600050629      *
005700050629     D    NR_campi     s              3u 0
005800050629
005900050629     D  Campi_Record   ds
006000090130     D    Dato_sk                  1000    DIM(50)
006100050629
006200050629     D  Campi_Lunghi   ds
006300110302     D    LunDato_sk                  5s 0 DIM(50)
006400050629
006500050629     D Campi_Numerici  ds
006600090130     D    DatoNum_sk                  1    DIM(50)
006700050629
006800050629     D Campi_Decimali  ds
006900110302     D    Decimal_sk                  3u 0 DIM(50)
007000050713      *
007100050713      * contatore di schiere
007200050713     D  sk             s              5i 0 INZ(0)
007300050713      *
007400050713      * ?------------------------------------------------------------------ */
007500050629     D Digits          C                   CONST('0123456789')
007600050628      * ?------------------------------------------------------------------ */
007700050628     c     *entry        plist
007800050629      * ?     Input
007900090130     c                   parm                    Separa_segmen
008000050629     c                   parm                    Decimal_Separ
008100090204     c                   parm                    Dati
008200050629      * ?     Output
008300050629     c                   parm                    Nr_campi
008400050629     c                   parm                    Campi_Record
008500050629     c                   parm                    Campi_Lunghi
008600050629     c                   parm                    Campi_Numerici
008700050629     c                   parm                    Campi_Decimali
008800110621     c                   parm                    esito
008900050713      **
009000050713      ** Pulizia DS da restituire
009100050713      **    (essendo una ridefinizione delle schiere --> pulizia schiere)
009200050629     c                   clear                   Nr_campi
009300050629     c                   clear                   Campi_Record
009400050629     c                   clear                   Campi_Lunghi
009500050629     c                   clear                   Campi_Numerici
009600050629     c                   clear                   Campi_Decimali
009700090204     c                   clear                   Campo_Nr
009800090204     c                   clear                   Num_decimali
009900050629      **
010000050628      **  Controlla dove trovare il separatore
010100050713      **  e si deve partire la prima volta dalla prima posizione
010200050713      **  impostando la lunghezza massima della stringa di carattere passata
010300050713      **  che compone il record. (Questo per non perdere tempo dopo l'ultimo
010400050713      **  carattere valido con dei blanks inutili)
010500050629     c                   eval      in_X_prima = 1
010600090204     c                   eval      Lun_dati = %Len(%TrimR(Dati))
010700090204      *
010800121008      *  Attenzione se NON viene passato nulla nel campo ....anche se obbligatorio
010900121008     c                   if        Lun_dati = 1
011000121008      *
011100121008      *  ...se è il divisore fra campi
011200121008     c                   if        %subst(Dati:Lun_dati:1) = Separa_segmen
011300121008     c                   end
011400121008      *
011500121008     c                   else
011600121008      *
011700090204      * esegue un controllo su l'ultimo carattere della stringa passata e se non c'è il
011800090204      *  separatore che ci si aspetta lo aggiunge per poter seguire lo split anche
011900090204      *  dell'ultimo campo.
012000090204      *
012100110224     c                   if        %subst(Dati:Lun_dati:1) <> Separa_segmen
012200110224     c                               or
012300110224     c                             %subst(Dati:Lun_dati:1) = Separa_segmen and
012400110224     c                             %subst(Dati:Lun_dati-1:1) = '?'
012500100311      *
012600100311      *  se c'è l'apostrofo
012700090204     c                   if        %subst(Dati:Lun_dati:1) = ''''
012800100311      *
012900100311      *   è apostrofo vero o è il fine segmento ?
013000100311     c                   if          Lun_dati > 1 and
013100100311     c                               %subst(Dati:Lun_dati - 1:1) <> '?'
013200100311     c                             or  Lun_dati = 1
013300090204     c                   eval      %subst(Dati:Lun_dati:1) = Separa_segmen
013400100311     c                   else
013500100311      *  deve trattarlo come apostrofo o accento carattere
013600100311     c                   eval      Lun_dati = Lun_dati + 1
013700100311     c                   eval      %subst(Dati:Lun_dati:1) = Separa_segmen
013800100311     c                   end
013900100311      *
014000090204     c                   else
014100090204     c                   eval      Lun_dati = Lun_dati + 1
014200090204     c                   eval      %subst(Dati:Lun_dati:1) = Separa_segmen
014300090204     c                   end
014400121008      **
014500090204     c                   end
014600050628      **
014700121008     c                   endIF
014800121008      **
014900050713      **  Esegue un loop byte x byte fino alla lunghezza massima
015000050713      **  per cercare dove si trova il separatore
015100050629     c                   do        Lun_dati      in_X
015200050628      **
015300050628      * ?   ( se trovato separatore imposta il nuovo campo )
015400050713      * ?     dal punto in cui si trova il primo byte non separatore fino al primo
015500050713      * ?     separatore trovato.
015600090205     c                   If        %Subst(Dati:in_X:1) = Separa_segmen
015700090205      **
015800090205     c                   If        in_X > 1
015900090205     c                   if        %Subst(Dati:(in_X-1):1) = '?'
016000090205      * non deve considerarlo come un separatore poichè il (?) precedente al carattere
016100090205      * speciale lo rende un carattere normale
016200090205     c                   goto      NO_separatore
016300090205     c                   end
016400090205     c                   end
016500050628      **
016600050629      * ?   ( Che campo sta analizzando ?? --> il campo numero x della stringa )
016700050713      **      si memorizza il nr.campo che sta elaborando e resetta i flags
016800050628     c                   add       1             Campo_Nr
016900050629     c                   eval      Campo_Vuoto    = 'N'
017000050629     c                   eval      Campo_Numerico = 'N'
017100050629     c                   eval      Campo_Decimali = 'N'
017200050629     c                   eval      Num_Decimali   =  0
017300050628      **
017400050628      **  Imposta i campi
017500050713      **   occorre impostare da quale byte iniziare della stringa da splittare
017600050713      **   per poi eseguire la SUBSTRING e recepire l'informazione contenuta.
017700050629     c                   if        Campo_Nr = 1
017800050713      * ?  Se è il primo campo è il punto da cui partire quindi è il 1 byte
017900050629     c                   eval      da_X = 1
018000050629     c                   else
018100050713      * ?   punto da cui partire successivamente
018200050629     c                   eval      da_X = in_X_prima + 1
018300050629     c                   end
018400050629      **
018500050713      **  Lunghezza del contenuto è il punto in cui si è trovato il separatore
018600050713      **   meno il byte da dove inizia l'informazione.
018700050629     c                   eval      Campo_Lungo = in_X - da_X
018800050629      * ?   ( Campo Vuoto )
018900050713      **   se la differenza è zero allora il campo è vuoto ossia ci sono
019000050713      **   2 separatori contigui.
019100050629     c                   if        Campo_Lungo = 0
019200050713      * pulisce il contenuto del campo e memorizza che il campo non contiene
019300050713      * alcun valore.
019400050629     c                   eval      Campo_Vuoto = 'S'
019500050629     c                   clear                   campo
019600050629
019700050713      * Se invece è diverso da zero allora c'è qualcosa nel campo
019800050628     c                   else
019900050628      * ?   ( imposta il dato rilevato nel campo )
020000050713      *  quindi per SUBSTRING rileva l'informazione e la memorizza sul CAMPO generico
020100050629     c                   eval      campo = %subst(Dati:da_X:Campo_Lungo)
020200050629
020300050629      * ?   ( controlla se ha solo numeri )
020400050713      * E' utile sapere se sono contenuti solo numeri e se ci sono dei decimali
020500050713      *  oppure ci sono caratteri alfanumerici.
020600050629     c                   eval      Campo_Numerico = 'S'
020700050629     c                   z-add     0             Num_decimali
020800050629     c                   Do        Campo_Lungo   X_Byte
020900050629     c                   eval      Byte = %subst(Campo:X_Byte:1)
021000050629     c     digits        check     Byte
021100050629     c                   if        %Found
021200050713      ** se non è un numero, è il separatore decimale ??????
021300110617     c                   if        Byte = Decimal_Separ or Byte = ','
021400110617     c                   if          Byte = ','
021500110617     c                   eval        Byte = '.'
021600110617     c                   end
021700050629     c                   eval      Campo_Decimali = 'S'
021800050629     c                   else
021900050713      ** se non lo è allora trattasi di campo alfanumerico.
022000050629     c                   eval      Campo_Numerico = 'N'
022100050629     c                   Leave
022200050629     c                   end
022300050629     c                   else
022400050713      **
022500050713      * se c'erano dei decimali quanti sono ???? Li conta
022600050713      ** tanti numeri podo il separatore decimale.
022700050629     c                   if        Campo_Decimali = 'S'
022800050629     c                   add       1             Num_decimali
022900050629     c                   end
023000050629     c                   end
023100050629     c                   endDo
023200050629      *
023300050628     c                   end
023400050628      **
023500050713      ** Le 5 definizioni di Output:
023600050629      ** Memorizza le informazioni di ciascun campo su schiere
023700050629     c                   z-add     Campo_Nr      Sk
023800090130     c                   if        Campo_Lungo > 1000
023900090130     c                   z-add     1000          LunDato_sk(Sk)
024000061214     c                   else
024100050629     c                   z-add     Campo_Lungo   LunDato_sk(Sk)
024200061214     c                   end
024300050629     c                   movel(p)  Campo         Dato_sk(Sk)
024400050629     c                   movel(p)  Campo_NumericoDatoNum_sk(Sk)
024500050629     c                   z-add     Num_Decimali  Decimal_sk(Sk)
024600050629      *
024700050713      ** Uscendo dal ciclo
024800050713      ** Memorizza la posizione del separatore per reiniziare un nuovo
024900050713      *   ciclo.
025000050629     c                   eval      in_X_prima = in_X
025100050629      **
025200090205     c     NO_separatore tag
025300090205      **
025400050628     c                   EndIf
025500050629
025600050628     c                   EndDo
025700050609
025800050629      * ?   ( imposta campi di ritorno da schiere )
025900050629     c                   eval      Nr_campi = Sk
026000050629
026100090212     c                   seton                                        RT
026200050628      * ?------------------------------------------------------------------ */
026300110621      * ?------------------------------------------------------------------ */
026400110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
026500110621      * ?------------------------------------------------------------------ */
026600110621     C     *pssr         BEGSR
026700110621     C
026800110621     C                   eval      esito = 'E'
026900110621     C                   ENDSR     '*CANCL'
027000110621     C
027100110621      *  ------------------------------------------------------------------ */
