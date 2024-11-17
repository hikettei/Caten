(in-package :caten/llm)

(defun list->hash-table (list)
  (let ((out (make-hash-table)))
    (loop for kv-pair in list do
      (setf (gethash (car kv-pair) out) (second kv-pair)))
    out))

(defun hash-table-revkv (hash-table)
  (let ((out (make-hash-table :test #'equal)))
    (loop for key in (alexandria:hash-table-keys hash-table) do
      (setf (gethash (gethash key hash-table) out) key))
    out))

(defparameter *byte2unicode*
  (list->hash-table
  `((33 "!")
    (34 "\"")
    (35 "#")
    (36 "$")
    (37 "%")
    (38 "&")
    (39 "'")
    (40 "(")
    (41 ")")
    (42 "*")
    (43 "+")
    (44 ",")
    (45 "-")
    (46 ".")
    (47 "/")
    (48 "0")
    (49 "1")
    (50 "2")
    (51 "3")
    (52 "4")
    (53 "5")
    (54 "6")
    (55 "7")
    (56 "8")
    (57 "9")
    (58 ":")
    (59 ";")
    (60 "<")
    (61 "=")
    (62 ">")
    (63 "?")
    (64 "@")
    (65 "A")
    (66 "B")
    (67 "C")
    (68 "D")
    (69 "E")
    (70 "F")
    (71 "G")
    (72 "H")
    (73 "I")
    (74 "J")
    (75 "K")
    (76 "L")
    (77 "M")
    (78 "N")
    (79 "O")
    (80 "P")
    (81 "Q")
    (82 "R")
    (83 "S")
    (84 "T")
    (85 "U")
    (86 "V")
    (87 "W")
    (88 "X")
    (89 "Y")
    (90 "Z")
    (91 "[")
    (92 "\\")
    (93 "]")
    (94 "^")
    (95 "_")
    (96 "`")
    (97 "a")
    (98 "b")
    (99 "c")
    (100 "d")
    (101 "e")
    (102 "f")
    (103 "g")
    (104 "h")
    (105 "i")
    (106 "j")
    (107 "k")
    (108 "l")
    (109 "m")
    (110 "n")
    (111 "o")
    (112 "p")
    (113 "q")
    (114 "r")
    (115 "s")
    (116 "t")
    (117 "u")
    (118 "v")
    (119 "w")
    (120 "x")
    (121 "y")
    (122 "z")
    (123 "{")
    (124 "|")
    (125 "}")
    (126 "~")
    (161 "¡")
    (162 "¢")
    (163 "£")
    (164 "¤")
    (165 "¥")
    (166 "¦")
    (167 "§")
    (168 "¨")
    (169 "©")
    (170 "ª")
    (171 "«")
    (172 "¬")
    (174 "®")
    (175 "¯")
    (176 "°")
    (177 "±")
    (178 "²")
    (179 "³")
    (180 "´")
    (181 "µ")
    (182 "¶")
    (183 "·")
    (184 "¸")
    (185 "¹")
    (186 "º")
    (187 "»")
    (188 "¼")
    (189 "½")
    (190 "¾")
    (191 "¿")
    (192 "À")
    (193 "Á")
    (194 "Â")
    (195 "Ã")
    (196 "Ä")
    (197 "Å")
    (198 "Æ")
    (199 "Ç")
    (200 "È")
    (201 "É")
    (202 "Ê")
    (203 "Ë")
    (204 "Ì")
    (205 "Í")
    (206 "Î")
    (207 "Ï")
    (208 "Ð")
    (209 "Ñ")
    (210 "Ò")
    (211 "Ó")
    (212 "Ô")
    (213 "Õ")
    (214 "Ö")
    (215 "×")
    (216 "Ø")
    (217 "Ù")
    (218 "Ú")
    (219 "Û")
    (220 "Ü")
    (221 "Ý")
    (222 "Þ")
    (223 "ß")
    (224 "à")
    (225 "á")
    (226 "â")
    (227 "ã")
    (228 "ä")
    (229 "å")
    (230 "æ")
    (231 "ç")
    (232 "è")
    (233 "é")
    (234 "ê")
    (235 "ë")
    (236 "ì")
    (237 "í")
    (238 "î")
    (239 "ï")
    (240 "ð")
    (241 "ñ")
    (242 "ò")
    (243 "ó")
    (244 "ô")
    (245 "õ")
    (246 "ö")
    (247 "÷")
    (248 "ø")
    (249 "ù")
    (250 "ú")
    (251 "û")
    (252 "ü")
    (253 "ý")
    (254 "þ")
    (255 "ÿ")
    (0 "Ā")
    (1 "ā")
    (2 "Ă")
    (3 "ă")
    (4 "Ą")
    (5 "ą")
    (6 "Ć")
    (7 "ć")
    (8 "Ĉ")
    (9 "ĉ")
    (10 "Ċ")
    (11 "ċ")
    (12 "Č")
    (13 "č")
    (14 "Ď")
    (15 "ď")
    (16 "Đ")
    (17 "đ")
    (18 "Ē")
    (19 "ē")
    (20 "Ĕ")
    (21 "ĕ")
    (22 "Ė")
    (23 "ė")
    (24 "Ę")
    (25 "ę")
    (26 "Ě")
    (27 "ě")
    (28 "Ĝ")
    (29 "ĝ")
    (30 "Ğ")
    (31 "ğ")
    (32 "Ġ")
    (127 "ġ")
    (128 "Ģ")
    (129 "ģ")
    (130 "Ĥ")
    (131 "ĥ")
    (132 "Ħ")
    (133 "ħ")
    (134 "Ĩ")
    (135 "ĩ")
    (136 "Ī")
    (137 "ī")
    (138 "Ĭ")
    (139 "ĭ")
    (140 "Į")
    (141 "į")
    (142 "İ")
    (143 "ı")
    (144 "Ĳ")
    (145 "ĳ")
    (146 "Ĵ")
    (147 "ĵ")
    (148 "Ķ")
    (149 "ķ")
    (150 "ĸ")
    (151 "Ĺ")
    (152 "ĺ")
    (153 "Ļ")
    (154 "ļ")
    (155 "Ľ")
    (156 "ľ")
    (157 "Ŀ")
    (158 "ŀ")
    (159 "Ł")
    (160 "ł")
    (173 "Ń"))))

(defparameter *unicode2byte* (hash-table-revkv *byte2unicode*))
