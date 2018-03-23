000100071127     D*--------------------------------------------------
000200071127     D* Procedure name: RaddoppiareApici
000300071127     D* Purpose:        Raddoppia gli apici in una stringa.
000400071127     D* Returns:        Stringa di output.
000500071127     P* Parameter:      piInput => Stringa di input.
000600071127     P* Parameter:      piInputLength => Lunghezza input
000700071127     P* Parameter:      piOutput => Stringa di output
000800071127     P* Parameter:      piOutputLength => Lunghezza output
000900071127     D*--------------------------------------------------
001000071127     D RaddoppiareApici...
001100071127     D                 PR         65535A   VARYING
001200071127     D  piInput                   65535A   OPTIONS(*VARSIZE)
001300071127     D  piInputLength                10I 0 CONST
001400071127     D  piOutput                  65535A   OPTIONS(*NOPASS:*VARSIZE)
001500071127     D  piOutputLength...
001600071127     D                               10I 0 OPTIONS(*NOPASS) CONST
