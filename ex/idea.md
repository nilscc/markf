MarkF is a markup-scripting language. The markup is very similiar to the
Markdown language ( http://en.wikipedia.org/wiki/Markdown ):

    My headline
    ===
    
    This is a codeblock with some Haskell in it:
    
        main = do
            putStrLn "This is the MarkF markup/scripting language!"

will get rendered in a html as:

    <h1>My headline</h1>
    
    <p>This is a codeblock with some Haskell in it:</p>
    
    <pre><code>
    main = do
        putStrLn "This is the MarkF markup/scripting language!"
    </code></pre>

The new thing about MarkF is it's scripting ability. Imagine your text like
this:

    This is some $template.

In your webserver code, you might have something like this:

    renderMarkF :: Text -> Html
    renderMarkF t = evalMarkF lookup (parseMarkF t)
      where
        lookup = [ ("template", text "text") ]

This will result in:

    This is some text.

Great! So we have simple templates in MarkF. So, what about scripting?

    Sinus of 1 is $(sin 1).

And your haskell code:

    mySin :: [String] -> String
    mySin [i] = show (sin (read i))
    
    renderMarkF :: Text -> Html
    renderMarkF t = evalMarkF lookup (parseMarkF t)
      where
        lookup = [ ("sin", function mySin) ]

So we get:

    Sinus of 1 is 0.841470984078965.

Isn't this awesome? And there is more:

For example "let" expressions:

    $(let result = sin 1)
    Sinus of 1 is $result.

And cool stuff like:

    $(next code (highlight "haskell"))
    
       main = do
           putStrLn "This is highlightet haskell code!"

Where `next' is just one of many functions which let you navigate through the
markup syntax tree of MarkF.
