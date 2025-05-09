function oppositeTheme(theme) {
    if (theme === "light") {
        return "dark";
    } else {
        return "light";
    }
}

function applySyntaxTheme(theme) {
    const lightSyntax = document.getElementById('light-syntax');
    const darkSyntax = document.getElementById('dark-syntax');

    if (theme === 'dark') {
        lightSyntax.disabled = true;
        darkSyntax.disabled = false;
    } else {
        lightSyntax.disabled = false;
        darkSyntax.disabled = true;
    }
}

function themeIcon(theme) {
    const fragment = (theme === 'dark' ? 'moon' : 'sun')
    return `/img/icons.svg#${fragment}`
}

document.addEventListener('DOMContentLoaded', function () {
    // ✅ Load saved theme or system preference
    const savedTheme = localStorage.getItem('theme');
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    const initialTheme = savedTheme || (prefersDark ? 'dark' : 'light');
    const initialIcon = themeIcon(initialTheme);
    console.log(`Initial Theme: ${initialTheme}`)
    applySyntaxTheme(initialTheme);
    document.documentElement.setAttribute('data-theme', initialTheme);

    const switchButton = document.getElementById('theme-switch');
    const iconUse = switchButton.querySelector('#theme-icon-use');

    // set icon initially
    applySyntaxTheme(initialTheme);
    iconUse.setAttribute('href', themeIcon(initialTheme))

    function switchTheme(e) {
        const currentTheme = document.documentElement.getAttribute('data-theme') || 'light';
        const newTheme = oppositeTheme(currentTheme);

        document.documentElement.setAttribute('data-theme', newTheme);
        localStorage.setItem('theme', newTheme);  // ✅ Save theme

        applySyntaxTheme(newTheme);
        iconUse.setAttribute('href', themeIcon(newTheme))
    }

    switchButton.addEventListener('click', switchTheme, false);
});
