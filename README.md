# Taller-2-FLP

### Gramática para clausulas en Forma Normal Conjuntiva (FNC):

<br>

**Representación basada en listas**
$$ \begin{split}
&\text{<FNC-exp> \ } := \text{ \ \  \ FNC \ <int> \ ( \ <and-exp> \ ) \ }

\\ \\

&\text{<and-exp> \ } := \text{\ \ (<or-exp>) \ | \ (<or-exp>) AND <and-exp>}

\\ \\

&\text{<or-exp> \ } := \text{\ \ <int> \ | \ <int> OR <or-exp>}

\end{split}$$

<br>
<br>

**Representación basada en datatypes**
$$ \begin{split}
&\text{<FNC-exp> \ } := \text{ \ \  \ FNC \ <int> \ ( \ <and-exp> \ ) \ }

\\ \\

&\text{<and-exp> \ } := \text{\ \ (<or-exp>) \ | \ } \textbf{(<and-exp>)} \text{ AND <and-exp>}

\\ \\

&\text{<or-exp> \ } := \text{\ \ <int>} \ | \ \textbf{<or-exp>} \text{ OR <or-exp>}

\end{split}$$