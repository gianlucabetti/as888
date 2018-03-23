000100090202      *-------------------------------------------------------------------------------
000200090202      * SUDDIVIDE in singoli campi una stringa
000300090202      *  i cui campi sono contigui e separati da un carattere particolare.
000400090202      *    AAAAAAAAA+BBBB+CCCC,CC+DDDDDDD+EEEEEE+....
000500090202      *  diventano così:
000600090202      *    AAAAAAAAA         --> campo1
000700090202      *    BBBB              --> campo2
000800090202      *    CCCC,CC           --> campo3
000900090202      *    DDDDDDD           --> campo4
001000090202      *    EEEEEE            --> campo5
001100090202      * ------------------------------------------------------------------------
001200090202      * Riceve:
001300090202      *  la stringa da suddividere       in  DATI
001400090202      *  in separatore x split dei campi in  Separa_segmen "+" (nell'esempio)
001500090202      *  i decimali sono separati da ,/. in  Decimal_Separ "," (nell'esempio)
001600090202      *
001700090202      * Restituisce:
001800090202      *   Nr_campi           --> Nr. di quanti campi c'erano nella stringa
001900090202      *     delle schiere correlate:
002000090202      *   Campi_Record       --> Le variabili contenute nei campi
002100090202      *   Campi_Lunghi       --> Lunghezza di ogni singolo campo
002200090202      *   Campi_Numerici     --> se il campo è Numerico o Alfa
002300090202      *   Campi_Decimali     --> se numerico con quanti decimali
002400090202      *
002500090202      *-------------------------------------------------------------------------------
002600050713      * ?Definizioni x gestire lo split del record passato
002700110621     d  esito          s              1
002800110302     D  Lun_dati       s              5u 0 INZ(0)
002900050629     D in_X_prima      s              5i 0 INZ(0)
003000050629     D  in_X           s              5i 0 INZ(0)
003100050629     D  da_X           s              5i 0 INZ(0)
003200050629     D Campo_Nr        s              3u 0 INZ(0)
003300090130     D Campo           s           1000    INZ(' ')
003400110302     D Campo_Lungo     s              5i 0 INZ(0)
003500050629     D Campo_Vuoto     s              1    INZ(' ')
003600050629     D Campo_Numerico  s              1    INZ(' ')
003700050629     D Campo_Decimali  s              1    INZ(' ')
003800110302     D Num_Decimali    s              3i 0 INZ(0)
003900050629     D  X_Byte         s              5i 0 INZ(0)
004000050629     D    Byte         s              1    INZ(' ')
004100050713      *
004200050713      * ?Dati di Input  (Record ricevuto dal pgm chiamante e da splittare)
004300090130     D  Dati           s           2048
004400050713     D Decimal_Separ   s              1
004500090130     D Separa_segmen   s              1
004600090204     D Lung_dati       s              5i 0
004700050610      *
004800050713      * ? Schiere di Output / Campi di Output
004900050713      * ? I dati da ripassare al chiamante devono essere :
005000050713      * ? 1 - il nr.dei campi splittati
005100090130      * ? 2 - una schiera contenente i campi splittati (lunghi max.1000 chrs)
005200050713      * ? 3 - una schiera che definisce la lunghezza di ciascun campo
005300050713      * ? 4 - una schiera che dice se il campo è numerico o alfa
005400050713      * ? 5 - una schiera che dice se ci sono dei decimali (se numerico)
005500050629      *
005600050629     D    NR_campi     s              3u 0
005700050629
005800050629     D  Campi_Record   ds
005900090130     D    Dato_sk                  1000    DIM(50)
006000050629
006100050629     D  Campi_Lunghi   ds
006200110302     D    LunDato_sk                  5s 0 DIM(50)
006300050629
006400050629     D Campi_Numerici  ds
006500090130     D    DatoNum_sk                  1    DIM(50)
006600050629
006700050629     D Campi_Decimali  ds
006800110302     D    Decimal_sk                  3u 0 DIM(50)
006900050713      *
007000050713      * contatore di schiere
007100050713     D  sk             s              5i 0 INZ(0)
007200050713      *
007300050713      * ?------------------------------------------------------------------ */
007400050629     D Digits          C                   CONST('0123456789')
007500050628      * ?------------------------------------------------------------------ */
007600050628     c     *entry        plist
007700050629      * ?     Input
007800090130     c                   parm                    Separa_segmen
007900050629     c                   parm                    Decimal_Separ
008000090204     c                   parm                    Dati
008100050629      * ?     Output
008200050629     c                   parm                    Nr_campi
008300050629     c                   parm                    Campi_Record
008400050629     c                   parm                    Campi_Lunghi
008500050629     c                   parm                    Campi_Numerici
008600050629     c                   parm                    Campi_Decimali
008700110621     c                   parm                    esito
008800050713      **
008900050713      ** Pulizia DS da restituire
009000050713      **    (essendo una ridefinizione delle schiere --> pulizia schiere)
009100050629     c                   clear                   Nr_campi
009200050629     c                   clear                   Campi_Record
009300050629     c                   clear                   Campi_Lunghi
009400050629     c                   clear                   Campi_Numerici
009500050629     c                   clear                   Campi_Decimali
009600090204     c                   clear                   Campo_Nr
009700090204     c                   clear                   Num_decimali
009800050629      **
009900050628      **  Controlla dove trovare il separatore
010000050713      **  e si deve partire la prima volta dalla prima posizione
010100050713      **  impostando la lunghezza massima della stringa di carattere passata
010200050713      **  che compone il record. (Questo per non perdere tempo dopo l'ultimo
010300050713      **  carattere valido con dei blanks inutili)
010400050629     c                   eval      in_X_prima = 1
010500090204     c                   eval      Lun_dati = %Len(%TrimR(Dati))
010600090204      *
010700121008      *  Attenzione se NON viene passato nulla nel campo ....anche se obbligatorio
010800121008     c                   if        Lun_dati = 1
010900121008      *
011000121008      *  ...se è il divisore fra campi
011100121008     c                   if        %subst(Dati:Lun_dati:1) = Separa_segmen
011200121008     c                   end
011300121008      *
011400121008     c                   else
011500121008      *
011600090204      * esegue un controllo su l'ultimo carattere della stringa passata e se non c'è il
011700090204      *  separatore che ci si aspetta lo aggiunge per poter seguire lo split anche
011800090204      *  dell'ultimo campo.
011900090204      *
012000110224     c                   if        %subst(Dati:Lun_dati:1) <> Separa_segmen
012100110224     c                               or
012200110224     c                             %subst(Dati:Lun_dati:1) = Separa_segmen and
012300110224     c                             %subst(Dati:Lun_dati-1:1) = '?'
012400100311      *
012500100311      *  se c'è l'apostrofo
012600090204     c                   if        %subst(Dati:Lun_dati:1) = ''''
012700100311      *
012800100311      *   è apostrofo vero o è il fine segmento ?
012900100311     c                   if          Lun_dati > 1 and
013000100311     c                               %subst(Dati:Lun_dati - 1:1) <> '?'
013100100311     c                             or  Lun_dati = 1
013200090204     c                   eval      %subst(Dati:Lun_dati:1) = Separa_segmen
013300100311     c                   else
013400100311      *  deve trattarlo come apostrofo o accento carattere
013500100311     c                   eval      Lun_dati = Lun_dati + 1
013600100311     c                   eval      %subst(Dati:Lun_dati:1) = Separa_segmen
013700100311     c                   end
013800100311      *
013900090204     c                   else
014000090204     c                   eval      Lun_dati = Lun_dati + 1
014100090204     c                   eval      %subst(Dati:Lun_dati:1) = Separa_segmen
014200090204     c                   end
014300121008      **
014400090204     c                   end
014500050628      **
014600121008     c                   endIF
014700121008      **
014800050713      **  Esegue un loop byte x byte fino alla lunghezza massima
014900050713      **  per cercare dove si trova il separatore
015000050629     c                   do        Lun_dati      in_X
015100050628      **
015200050628      * ?   ( se trovato separatore imposta il nuovo campo )
015300050713      * ?     dal punto in cui si trova il primo byte non separatore fino al primo
015400050713      * ?     separatore trovato.
015500090205     c                   If        %Subst(Dati:in_X:1) = Separa_segmen
015600090205      **
015700090205     c                   If        in_X > 1
015800090205     c                   if        %Subst(Dati:(in_X-1):1) = '?'
015900090205      * non deve considerarlo come un separatore poichè il (?) precedente al carattere
016000090205      * speciale lo rende un carattere normale
016100090205     c                   goto      NO_separatore
016200090205     c                   end
016300090205     c                   end
016400050628      **
016500050629      * ?   ( Che campo sta analizzando ?? --> il campo numero x della stringa )
016600050713      **      si memorizza il nr.campo che sta elaborando e resetta i flags
016700050628     c                   add       1             Campo_Nr
016800050629     c                   eval      Campo_Vuoto    = 'N'
016900050629     c                   eval      Campo_Numerico = 'N'
017000050629     c                   eval      Campo_Decimali = 'N'
017100050629     c                   eval      Num_Decimali   =  0
017200050628      **
017300050628      **  Imposta i campi
017400050713      **   occorre impostare da quale byte iniziare della stringa da splittare
017500050713      **   per poi eseguire la SUBSTRING e recepire l'informazione contenuta.
017600050629     c                   if        Campo_Nr = 1
017700050713      * ?  Se è il primo campo è il punto da cui partire quindi è il 1 byte
017800050629     c                   eval      da_X = 1
017900050629     c                   else
018000050713      * ?   punto da cui partire successivamente
018100050629     c                   eval      da_X = in_X_prima + 1
018200050629     c                   end
018300050629      **
018400050713      **  Lunghezza del contenuto è il punto in cui si è trovato il separatore
018500050713      **   meno il byte da dove inizia l'informazione.
018600050629     c                   eval      Campo_Lungo = in_X - da_X
018700050629      * ?   ( Campo Vuoto )
018800050713      **   se la differenza è zero allora il campo è vuoto ossia ci sono
018900050713      **   2 separatori contigui.
019000050629     c                   if        Campo_Lungo = 0
019100050713      * pulisce il contenuto del campo e memorizza che il campo non contiene
019200050713      * alcun valore.
019300050629     c                   eval      Campo_Vuoto = 'S'
019400050629     c                   clear                   campo
019500050629
019600050713      * Se invece è diverso da zero allora c'è qualcosa nel campo
019700050628     c                   else
019800050628      * ?   ( imposta il dato rilevato nel campo )
019900050713      *  quindi per SUBSTRING rileva l'informazione e la memorizza sul CAMPO generico
020000050629     c                   eval      campo = %subst(Dati:da_X:Campo_Lungo)
020100050629
020200050629      * ?   ( controlla se ha solo numeri )
020300050713      * E' utile sapere se sono contenuti solo numeri e se ci sono dei decimali
020400050713      *  oppure ci sono caratteri alfanumerici.
020500050629     c                   eval      Campo_Numerico = 'S'
020600050629     c                   z-add     0             Num_decimali
020700050629     c                   Do        Campo_Lungo   X_Byte
020800050629     c                   eval      Byte = %subst(Campo:X_Byte:1)
020900050629     c     digits        check     Byte
021000050629     c                   if        %Found
021100050713      ** se non è un numero, è il separatore decimale ??????
021200110617     c                   if        Byte = Decimal_Separ or Byte = ','
021300110617     c                   if          Byte = ','
021400110617     c                   eval        Byte = '.'
021500110617     c                   end
021600050629     c                   eval      Campo_Decimali = 'S'
021700050629     c                   else
021800050713      ** se non lo è allora trattasi di campo alfanumerico.
021900050629     c                   eval      Campo_Numerico = 'N'
022000050629     c                   Leave
022100050629     c                   end
022200050629     c                   else
022300050713      **
022400050713      * se c'erano dei decimali quanti sono ???? Li conta
022500050713      ** tanti numeri podo il separatore decimale.
022600050629     c                   if        Campo_Decimali = 'S'
022700050629     c                   add       1             Num_decimali
022800050629     c                   end
022900050629     c                   end
023000050629     c                   endDo
023100050629      *
023200050628     c                   end
023300050628      **
023400050713      ** Le 5 definizioni di Output:
023500050629      ** Memorizza le informazioni di ciascun campo su schiere
023600050629     c                   z-add     Campo_Nr      Sk
023700090130     c                   if        Campo_Lungo > 1000
023800090130     c                   z-add     1000          LunDato_sk(Sk)
023900061214     c                   else
024000050629     c                   z-add     Campo_Lungo   LunDato_sk(Sk)
024100061214     c                   end
024200050629     c                   movel(p)  Campo         Dato_sk(Sk)
024300050629     c                   movel(p)  Campo_NumericoDatoNum_sk(Sk)
024400050629     c                   z-add     Num_Decimali  Decimal_sk(Sk)
024500050629      *
024600050713      ** Uscendo dal ciclo
024700050713      ** Memorizza la posizione del separatore per reiniziare un nuovo
024800050713      *   ciclo.
024900050629     c                   eval      in_X_prima = in_X
025000050629      **
025100090205     c     NO_separatore tag
025200090205      **
025300050628     c                   EndIf
025400050629
025500050628     c                   EndDo
025600050609
025700050629      * ?   ( imposta campi di ritorno da schiere )
025800050629     c                   eval      Nr_campi = Sk
025900050629
026000090212     c                   seton                                        RT
026100050628      * ?------------------------------------------------------------------ */
026200110621      * ?------------------------------------------------------------------ */
026300110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
026400110621      * ?------------------------------------------------------------------ */
026500110621     C     *pssr         BEGSR
026600110621     C
026700110621     C                   eval      esito = 'E'
026800110621     C                   ENDSR     '*CANCL'
026900110621     C
027000110621      *  ------------------------------------------------------------------ */
