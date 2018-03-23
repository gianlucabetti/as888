000100011011      *
000200060615      *  Routine che restituisce cap di stampa su etichetta  DPD
000300061013      *
000400061013      *  ATTENZIONE: Il passaggio del terzo parametro (INPNAZ=Nazione di
000500061013      *  appartenenza del cap passato) è facoltativo.
000600061013      *  Serve per la stampa delle etichette export dpd dirette in Olanda
000700061013      *  per via della differenza di struttura del cap olandese fra come
000800061013      *  esso è nella realtà (e anche nel cappario Bartolini) e fra come
000900061013      *  invece è gestito dal cappario DPD GEOPOST.
001000080128      *
001100080128      *  -----> Dal 28/01/2008 non si fanno più eccezioni per l'Olanda e
001200080128      *         quindi non è più necessario passare il cap a questo pgm
001201081024      *  -----> Dal 24/10/2008 Il cap Irlandese dovrà essere stampato con
001202081024      *         la costante '0000001'. Quindi viene riutilizzato il passaggi
001203081024      *         del terzo parametro
001300061013      *
001400061013      *
001500060615      * --------------------------------------------------------
001600011011      *
001700011011
001800011011     H DECEDIT('0,') DATEDIT(*DMY.)
001900011011
002000011011     D* Campi di lavoro
002100060615     D w9              s              1    dim(9)
002200060615     D w7NEW           s              1    dim(7)
002300060615     D w7              s              1    dim(7)
002400060615     c*--------------------------------------------------------------
002500060615     C     *ENTRY        PLIST
002600060615     C                   Parm                    inpcap            9
002700060615     C                   Parm                    outcapDPD         7
002800061013     C                   Parm                    inpnaz            3
002900061013     c*
003000061013     c* verifico se passato il terzo parametro
003100061013     c                   if        %parms<3
003200061013     c                   clear                   winpnaz           3
003300061013     c                   else
003400061013     c                   movel     inpnaz        winpnaz
003500061013     c                   endif
003600061013     c* Se passato cap olandese devo abblencare i caratteri alfabetici prese
003700061013     c* nti dopo i primi 4 byte
003800080128     c* 28/01/2008: ora non più, anche la dpd ora vuole il cap completo
003900080128     c******             if        winpnaz = 'NL '
004000080128     c******             eval      %subst(inpcap:5:5)= *blanks
004100080128     c******             endif
004101081024     c* Se passato cap Irlandese imposto fisso 0000001
004102081024     c                   if        winpnaz ='IRL'
004103081024     c                   eval      inpcap='0000001  '
004104081024     c                   endif
004200060615     c
004300060615     c                   movea     inpcap        w9
004400060615     c                   z-add     0             yy                2 0
004500060615     c
004600060615     c                   do        9             xx                2 0
004700060615     c                   if        w9(xx)<>' '
004800060615     c                   add       1             yy
004900060615     c                   movel     w9(xx)        w7new(yy)
005000060615     c                   endif
005100060615     c* max 7 caratteri
005200060615     c                   if        yy=7
005300060615     c                   leave
005400060615     c                   endif
005500060615     c                   enddo
005600060615     c
005700060615     c                   if        yy=7
005800060615     c                   movea     w7new         outcapDPD
005900060615     c                   else
006000060615     c
006100061013     c* Trovo la posizione iniziale dove impostare il campo
006200060615     c     8             sub       yy            xx
006300060615     c                   clear                   outcapDPD
006400060615     c                   movea     w7new         w007a             7
006500060615     c                   eval      %subst(outcapdpd:xx:yy)=w007a
006600060615     c
006700060615     c                   do        7             xx
006800060615     c                   if        %subst(outcapdpd:xx:1)=' '
006900060615     c                   eval      %subst(outcapdpd:xx:1)='0'
007000060615     c                   endif
007100060615     c                   enddo
007200060615     c                   endif
007300011011
007400011011      * Fine programma
007500060615     C                   seton                                        lr
007600011011
